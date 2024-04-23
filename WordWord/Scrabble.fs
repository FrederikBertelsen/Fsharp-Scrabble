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
        if getPlayerCount st = 1 then
            true
        else
            (getOurPlayerId st) = getCurrentTurn st % getPlayerCount st

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
    
    let idToChar (id: uint) (pieces: Map<uint32, tile>) : char =
        let tile = Map.find id pieces
        let (char, _) = Set.minElement tile
        char
        
    let rotateList lst =
        List.tail lst @ [lst[0]]
    
    let emptyBoardMove (st: State.state) (pieces: Map<uint32, tile>) =
        let handList = MultiSet.toList (State.getHand st)
        let charList = List.map (fun id -> idToChar id pieces) handList

        forcePrint $"%A{charList}"
        
        
        let rec goDown (str:string) (dict:Dictionary.Dict) (charL:List<char>) : (bool * string) =
            let o = Dictionary.step charL[0] dict
            forcePrint (str + string charL[0] + "\n")
            match o with
            | Some (b, newDict) ->
                if b then
                    // if we find a word we return it
                    forcePrint "found word"
                    (true, (str + (string charL[0])))
                else
                    if List.tail charL = [] then
                        (false, "")
                    else
                        rotate (str + string charL[0]) newDict (List.tail charL) (List.tail charL).Length
            | _ -> (false, "")
            
                        
                        
        and rotate str dict lst (amountToRotate: int) : (bool * string) =
            let o = goDown str dict lst
            if fst o then
                o
            else
                if amountToRotate <> 0 then
                    let rec tryToRotate l a =
                        let  p = rotate str dict (rotateList lst) (a-1)
                        if fst p || a = 0 then
                            p
                        else
                            tryToRotate (rotateList l) a
                    tryToRotate lst amountToRotate
                else
                    o
                    
        
        
        
        
        
        
        
        
        
        
        
        
        // let rec getNext (charL:List<char>) (dict: Dictionary.Dict) (acc:string) (length:int ): string =
        //     let o = Dictionary.step charL[0] dict
        //     
        //     forcePrint (acc + string charL[0] + "\n")
        //     forcePrint $"%A{length}"
        //     match o with
        //     | Some (b, newDict) ->
        //         let newAcc = acc + (string charL[0])
        //         let newList = List.tail charL
        //         if b then
        //             // if we find a word we return it
        //             forcePrint "found word"
        //             newAcc
        //         else
        //             // if we haven't found a word, but it is possible to find one with the letters we already have
        //             (getNext newList newDict newAcc (length - 1))
        //             // if  = acc then
        //             //     let rotatedList = List.tail charL @ [charL[0]]
        //             //     let rec wordNotFound newList dict acc length =
        //             //         getNext newList dict acc length
        //                     
        //                 
        //                 
        //             // if length = 0 then
        //             //     acc
        //             // else 
        //             //     
        //             //     (getNext lst dict acc (length - 1))
        //             
        //     | _ ->
        //         // if it is not possible to find a word with the letter we have already used
        //         if length = 0 then
        //             forcePrint "didnt find word"
        //             acc
        //         else 
        //             (getNext (rotateList charL) dict acc (length - 1))
        let p = rotate "" (State.getDictionary st) charList charList.Length
        // System.Environment.Exit(0)
        (SMPlay [],[])
        //let rec dictStep st dict rest =
            
    
    let move st pieces = (SMPass, [])
    let calculateNextMove (st: State.state) (pieces: Map<uint32, tile>) (isHumanPlayer: bool) =
        if isHumanPlayer then
            let input = System.Console.ReadLine()

            if input.Equals "" then
                (SMPass, [])
            else
                let move = RegEx.parseMove input
                ((SMPlay move), move)
        else
            
            // --- bot logic here ---
            // For each letter on board, check if we can make a word with the letters in hand
            // Check if that word will create more words when put into board
            // if so, skip it
            // send word found
            if Map.isEmpty (State.getPlacedTiles st) then
                emptyBoardMove st pieces
            else
                move st pieces
            //(SMPass, [])
            
            
            


module Scrabble =
    open System.Threading

    let isHumanPlayer = false

    let playGame cStream (pieces: Map<uint32, tile>) (st: State.state) =
        let rec aux (st: State.state) =
            if State.isOurTurn st then
                forcePrint $"\n----------------- OUR TURN (%d{State.getCurrentTurn st}) -----------------\n\n"
                Print.printHand pieces (State.getHand st)

                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint
                    "Input move (format '(<x> <y> <piece id><char><points> )*', no spaces between last inputs)\n\n"

                // calculate the move that we will make
                let nextMove = BotLogic.calculateNextMove st pieces isHumanPlayer
                
                // send move to server
                forcePrint "Sending move to server... "
                send cStream (fst nextMove)
                forcePrint $"(Sent!)\nMove sent: %A{nextMove}\n\n"

            else
                // if it isn't our turn, do nothing
                forcePrint $"\n--------------- NOT OUR TURN (%d{State.getCurrentTurn st}) ---------------\n\n"
                ()



            // get server response
            forcePrint "Waiting for server response... "
            let msg = recv cStream
            forcePrint $"(Received!)\nServer response: %A{msg}\n\n"

            // act on server response
            match msg with
            | RCM(CMPlaySuccess(moveSequence, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                // update state values
                let currentTurn = State.incrementCurrentTurn (State.getCurrentTurn st)

                let placedTiles = State.AddTilesToPlacedTiles (State.getPlacedTiles st) moveSequence
                let updatedHand = State.removeTilesFromHand (State.getHand st) moveSequence
                let updatedHand' = State.addTilesToHand updatedHand newPieces

                let st' = State.updateState st updatedHand' currentTurn placedTiles
                aux st'
            | RCM(CMPlayed(playerId, moveSequence, points)) ->
                (* Successful play by other player. Update your state *)

                // update state values
                let currentTurn = State.incrementCurrentTurn (State.getCurrentTurn st)

                let placedTiles = State.AddTilesToPlacedTiles (State.getPlacedTiles st) moveSequence

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
                // (* Turn passed. Update your state *)
                
                // update state values
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
