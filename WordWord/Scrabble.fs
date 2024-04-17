namespace WordWord

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          tiles: Map<uint32, tile>
          placedTiles: Map<coord, char>
          hand: MultiSet.MultiSet<uint32>
          dictionary: ScrabbleUtil.Dictionary.Dict
          playerCount: int
          ourPlayerId: int
          currentTurn: int }

    let mkState board tiles hand dictionary playerCount ourPlayerId currentTurn =
        { board = board
          tiles = tiles
          placedTiles = Map.empty<coord, char>
          hand = hand
          dictionary = dictionary
          playerCount = (playerCount |> int)
          ourPlayerId = (ourPlayerId |> int)
          currentTurn = (currentTurn |> int) }

    let getBoard st = st.board
    let getTiles st = st.tiles
    let getPlacedTiles st = st.placedTiles
    let getHand st = st.hand
    let getDictionary st = st.dictionary

    let getPlayerCount st = st.playerCount
    let getOurPlayerId st = st.ourPlayerId
    let getCurrentTurn st = st.currentTurn

    let incrementCurrentTurn currentTurn = currentTurn + 1

    let isOurTurn st =
        (getOurPlayerId st - 1) = getCurrentTurn st % getPlayerCount st

    let updateState st hand currentTurn placedTiles =
        { board = getBoard st
          dictionary = getDictionary st
          ourPlayerId = getOurPlayerId st
          playerCount = getPlayerCount st
          tiles = getTiles st
          hand = hand
          currentTurn = currentTurn
          placedTiles = placedTiles }

    let removeTilesFromHand hand tilesToRemove =
        let tileIdsToRemove = List.map (fun (_, (tileId, (_, _))) -> tileId) tilesToRemove

        List.fold (fun updatedHand tileToRemove -> MultiSet.removeSingle tileToRemove updatedHand) hand tileIdsToRemove

    let addTilesToHand hand tilesToAdd =
        List.fold
            (fun handToUpdate (tileIdToAdd, quantity) -> MultiSet.add tileIdToAdd quantity handToUpdate)
            hand
            tilesToAdd

    let AddTilesToPlacedTiles (placedTiles: Map<coord, char>) (moveSequence: (coord * (uint32 * (char * int))) list) =
        List.fold
            (fun updatedPlacedTiles (coordinate, (_, (char, _))) -> Map.add coordinate char updatedPlacedTiles)
            placedTiles
            moveSequence

module BotLogic =
    let calculateNextMove st pieces isHumanPlayer =
        if isHumanPlayer then
            let input = System.Console.ReadLine()

            if input.Equals "" then
                (SMPass, [])
            else
                let move = RegEx.parseMove input
                ((SMPlay move), move)
        else
            // --- bot logic here ---
            (SMPass, [])

module Scrabble =
    open System.Threading

    let isHumanPlayer = true

    let playGame cStream pieces (st: State.state) =
        let rec aux (st: State.state) =
            let mutable nextMove = (SMPass, [])
            if State.isOurTurn st then
                forcePrint "\n------------------- OUR TURN -------------------\n\n"
                Print.printHand pieces (State.getHand st)

                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint
                    "Input move (format '(<x> <y> <piece id><character><points> )*', no spaces between the last inputs)\n\n"

                // calculate the move that we will make
                nextMove <- BotLogic.calculateNextMove st pieces isHumanPlayer
            else
                // if it isn't our turn, do nothing
                forcePrint "\n----------------- NOT OUR TURN -----------------\n\n"
                ()

            // send move to server
            // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.getOurPlayerId st) (snd nextMove)) // keep the debug lines. They are useful.
            send cStream (fst nextMove)

            // get server response
            let msg = recv cStream
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.getOurPlayerId st) (snd nextMove)) // keep the debug lines. They are useful.

            // act on server response
            match msg with
            | RCM(CMPlaySuccess(moveSequence, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                // update state values
                let placedTiles = State.AddTilesToPlacedTiles (State.getPlacedTiles st) moveSequence
                let updatedHand = State.removeTilesFromHand (State.getHand st) moveSequence
                let updatedHand' = State.addTilesToHand updatedHand newPieces
                let currentTurn = State.incrementCurrentTurn (State.getCurrentTurn st)

                let st' = State.updateState st updatedHand' currentTurn placedTiles
                aux st'
            | RCM(CMPlayed(playerId, moveSequence, points)) ->
                (* Successful play by other player. Update your state *)

                // update state values
                let placedTiles = State.AddTilesToPlacedTiles (State.getPlacedTiles st) moveSequence
                let currentTurn = State.incrementCurrentTurn (State.getCurrentTurn st)

                let st' = State.updateState st (State.getHand st) currentTurn placedTiles
                aux st'
            | RCM(CMPlayFailed(playerId, moveSequence)) ->
                (* Failed play. Update your state *)

                // update state values
                let currentTurn = State.incrementCurrentTurn (State.getCurrentTurn st)

                let st' =
                    State.updateState st (State.getHand st) currentTurn (State.getPlacedTiles st)

                aux st'
            | RCM(CMPassed _) -> 
                let currentTurn = State.incrementCurrentTurn (State.getCurrentTurn st)

                let st' =
                    State.updateState st (State.getHand st) currentTurn (State.getPlacedTiles st)

                aux st'
            | RCM(CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st

        // start game loop
        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board tiles handSet dict numPlayers playerNumber playerTurn)
