namespace WordWord

open System
open System.Net.Mime
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
        |> MultiSet.fold (fun _ x i -> debugPrint $"%d{x} -> (%A{Map.find x pieces}, %d{i})\n") ()

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
        char (id + 64u)
        //let tile = Map.find id pieces
        //let (char, _) = Set.maxElement tile
        //char
        
    let charToId (char: char) (pieces: Map<uint32, tile>) : uint32 =
        (uint32 char) - 64u
        //Map.findKey (fun _ tile -> Set.exists (fun (c, _) -> c = char) tile) pieces

    let rotateList lst =
        List.tail lst @ [lst[0]]
        
        
    let spaceAvailable (str:String) (c:char) verPrefix verSuffix horPrefix horSuffix =
        if c = '0' then
            ((true, false), str)
        else
            if (str.Contains(c)) then
                let parts = str.Split(c)
                if Array.length parts > 2 then
                    ((false, false), str)
                else
                    if String.length parts[0] < verPrefix && String.length parts[1] < verSuffix &&
                       verPrefix <> 0 && verSuffix <> 0 then
                        ((true, false), str)
                    elif String.length parts[0] < horPrefix && String.length parts[1] < horSuffix &&
                         horPrefix <> 0 && horSuffix <> 0 then
                        ((true, true), str)
                    else
                        ((false,false), str)
            else
                ((false, false), str)
        

    let rec goDown (str:string) (dict:Dictionary.Dict) (charL:List<char>) (c:char) verPrefix verSuffix horPrefix horSuffix : ((bool*bool) * string) =
        let o = Dictionary.step charL[0] dict
        match o with
        | Some (b, newDict) ->
            if b then
                spaceAvailable (str + (string charL[0])) c verPrefix verSuffix horPrefix horSuffix
            else
                if List.tail charL = [] then
                    ((false,false), str)
                else
                    rotate (str + string charL[0]) newDict (List.tail charL) c verPrefix verSuffix horPrefix horSuffix
        | _ -> ((false, false), "")
        
                    
                    
    and rotate (str:string) dict (lst:List<char>) (c:char) verPrefix verSuffix horPrefix horSuffix : ((bool*bool) * string) =
        let rec tryToRotate (l:List<char>) rotateAmount =
            let  result = goDown str dict l c verPrefix verSuffix horPrefix horSuffix
            if fst (fst result) || rotateAmount = 0 then
                result
            else
                tryToRotate  (rotateList l) (rotateAmount-1)
        tryToRotate lst (lst.Length-1)

    let translateIdToTile id (pieces  : Map<uint32, tile>) (suffix: bool) (horizontal: bool) (refPos: (int*int)) count: coord * (uint * (char * int)) =
        if suffix then
            if horizontal then
                let coord = ((fst refPos) + 1 + count, snd refPos)
                let til = Map.find id pieces
                (coord , (id , Set.minElement til))
            else 
                let coord = (fst refPos, (snd refPos) + 1 + count)
                let til = Map.find id pieces
                (coord , (id , Set.minElement til))
        else 
            if horizontal then
                let coord = ((fst refPos) - (1 + count), snd refPos)
                let til = Map.find id pieces
                (coord , (id , Set.minElement til))
            else 
                let coord = (fst refPos, (snd refPos) - (1 + count))
                let til = Map.find id pieces
                (coord , (id , Set.minElement til))

    let translateStringToPlay (str: string) (suffix: bool) (horizontal: bool) (refPos: (int*int)) (pieces  : Map<uint32, tile>) =  
        let charList = Seq.toList str
        let idList = List.map (fun char -> charToId char pieces) charList

        let foldTranslateIdToTile (pieces : Map<uint32, tile>) (suffix : bool) (horizontal : bool) (refPos : int * int) (count, result) id =
            let translatedTile = translateIdToTile id pieces suffix horizontal refPos count
            (count + 1, translatedTile :: result)

        if suffix then
            snd (List.fold (foldTranslateIdToTile pieces suffix horizontal refPos) (0, []) idList)
        else
            snd (List.fold (foldTranslateIdToTile pieces suffix horizontal refPos) (0, []) (List.rev idList))

    let emptyBoardMove (st: State.state) (pieces: Map<uint32, tile>) =
        let handList = MultiSet.toList (State.getHand st)
        let charList = handList |> List.filter (fun id -> id <> 0u) |> List.map (fun id -> idToChar id pieces)        
                 
        let p = rotate "" (State.getDictionary st) charList '0' 7 7 7 7
        if fst (fst p) then
            (SMPlay (translateStringToPlay (snd p) true true (-1, 0) pieces), translateStringToPlay (snd p) true true (-1, 0) pieces)
        else 
            (SMPass,[])
        //let rec dictStep st dict rest =
        

    let calculateLimits (placedTiles:  Map<coord,char>) (handSize: uint) : List<coord * char * (int * int * int * int)>=
        let rec inner (placedTilesList: List<coord * char>) (currentCoord: coord) (maxHorizontalPrefix:int) (maxHorizontalSuffix:int) (maxVerticalPrefix:int) (maxVerticalSuffix:int)  : (int * int * int * int) =
            match placedTilesList with
            | [] -> (maxHorizontalPrefix, maxHorizontalSuffix, maxVerticalPrefix, maxVerticalSuffix)
            | (obstacleCoord, _) :: tail ->
                // Horizontal check
                let maxHorizontalPrefix, maxHorizontalSuffix = 
                    if (snd currentCoord) = (snd obstacleCoord) then
                        let distanceX: int = abs ((fst obstacleCoord) - (fst currentCoord))
                        
                        // Calculate prefix and suffix limits (in line)
                        let maxHorizontalPrefix =
                             if (fst obstacleCoord) < (fst currentCoord) && distanceX <= maxHorizontalPrefix then
                                 distanceX - 1
                             else
                                 maxHorizontalPrefix
                        
                        let maxHorizontalSuffix = 
                            if (fst obstacleCoord) > (fst currentCoord) && distanceX <= maxHorizontalSuffix then
                                distanceX - 1
                            else
                                maxHorizontalSuffix
                                
                        (maxHorizontalPrefix, maxHorizontalSuffix)
                    // Adjust prefix and suffix if there are obstacles adjacent to the row of a potential horizontal word
                    elif (snd obstacleCoord) = (snd currentCoord) - 1 || (snd obstacleCoord) = (snd currentCoord) + 1 then
                        let maxHorizontalPrefix =
                            if (fst obstacleCoord) < (fst currentCoord) then
                                min ((fst currentCoord) - (fst obstacleCoord) - 1) maxHorizontalPrefix                        
                            else
                                maxHorizontalPrefix

                        let maxHorizontalSuffix =
                            if (fst obstacleCoord) > (fst currentCoord) then
                                min ((fst obstacleCoord) - (fst currentCoord) - 1) maxHorizontalSuffix
                            else
                                maxHorizontalSuffix
                       
                        (maxHorizontalPrefix, maxHorizontalSuffix)
                    else
                        (maxHorizontalPrefix, maxHorizontalSuffix)
                
                // Vertical check
                let maxVerticalPrefix, maxVerticalSuffix =
                    if (fst currentCoord) = (fst obstacleCoord) then
                        let distanceY: int = abs ((snd obstacleCoord) - (snd currentCoord))
                        
                        // Calculate prefix and suffix limits (in line)
                        let maxVerticalPrefix =
                            if (snd obstacleCoord) < (snd currentCoord) && distanceY <= maxVerticalPrefix then
                                distanceY - 1
                            else
                                maxVerticalPrefix
                            
                        let maxVerticalSuffix =
                            if (snd obstacleCoord) > (snd currentCoord) && distanceY <= maxVerticalSuffix then
                                distanceY - 1
                            else
                                maxVerticalSuffix
                                
                        (maxVerticalPrefix, maxVerticalSuffix)
                    // Adjust prefix and suffix if there are obstacles adjacent to the column of a potential vertical word
                    elif (fst obstacleCoord) = (fst currentCoord) - 1 || (fst obstacleCoord) = (fst currentCoord) + 1 then
                        let maxVerticalPrefix = 
                            if (snd obstacleCoord) < (snd currentCoord) then
                                min ((snd currentCoord) - (snd obstacleCoord) - 1) maxVerticalPrefix
                            else
                                maxVerticalPrefix
                        let maxVerticalSuffix = 
                            if (snd obstacleCoord) > (snd currentCoord) then
                                min ((snd obstacleCoord) - (snd currentCoord) - 1) maxVerticalSuffix
                            else
                                maxVerticalSuffix
                                
                        (maxVerticalPrefix, maxVerticalSuffix)
                    else
                        (maxVerticalPrefix, maxVerticalSuffix)
              
                inner tail currentCoord maxHorizontalPrefix maxHorizontalSuffix maxVerticalPrefix maxVerticalSuffix

        let placedTilesList = Map.toList placedTiles
        let handSizeInt = int handSize
        
        List.map (fun (currentCoord, currentChar) ->
            let maxHorizontalPrefix, maxHorizontalSuffix, maxVerticalPrefix, maxVerticalSuffix = inner placedTilesList currentCoord handSizeInt handSizeInt handSizeInt handSizeInt
            (currentCoord, currentChar, (maxHorizontalPrefix, maxHorizontalSuffix, maxVerticalPrefix, maxVerticalSuffix))
        ) placedTilesList
        
    
    let move (st: State.state) (pieces: Map<uint32, tile>) =
        let placementLimits = calculateLimits (State.getPlacedTiles st) (MultiSet.size (State.getHand st))
        let handList = MultiSet.toList (State.getHand st)
        let charList = handList |> List.filter (fun id -> id <> 0u) |> List.map (fun id -> idToChar id pieces)
        
        let rec findWord (lst: List<coord * char * (int*int*int*int)>) =
            let item = lst[0]
            match item with
            | (coord, c, tup) ->
                match tup with
                | (HP, HS, VP, VS) ->
                    let handWithBoardLetter = c :: charList
                    let boolWordPair = rotate "" (State.getDictionary st) handWithBoardLetter c VP VS HP HS
                    
                    if fst (fst boolWordPair) then
                        //todo: split word on char, and run translateStringToPlay on both sides and combine them
                        let wordSplit = (snd boolWordPair).Split(c)
                        let prefixPlay = (translateStringToPlay wordSplit[0] false (snd (fst boolWordPair)) coord pieces)
                        let suffixPlay = (translateStringToPlay ((Array.tail wordSplit)[0]) true (snd (fst boolWordPair)) coord pieces)
                        (SMPlay (prefixPlay @ suffixPlay), (prefixPlay @ suffixPlay))
                    else
                        if List.tail lst = [] then
                            (SMPass, [])
                        else
                            findWord (List.tail lst)
                | (_,_,_,_) -> (SMPass, [])
            | (_,_,_) -> (SMPass, [])
        
        findWord placementLimits
            
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
            if Map.isEmpty (State.getPlacedTiles st) then
                emptyBoardMove st pieces
            else
                move st pieces            
            
            


module Scrabble =
    let isHumanPlayer = false

    let playGame cStream (pieces: Map<uint32, tile>) (st: State.state) =
        let rec aux (st: State.state) =
            if State.isOurTurn st then
                Print.printHand pieces (State.getHand st)
                
                // calculate the move that we will make
                let nextMove = BotLogic.calculateNextMove st pieces isHumanPlayer
                
                // send move to server
                send cStream (fst nextMove)

            else
                // if it isn't our turn, do nothing
                ()



            // get server response
            let msg = recv cStream
            debugPrint $"(Received!)\nServer response: %A{msg}\n\n"

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
            | RGPE err ->
                debugPrint $"Gameplay Error:\n%A{err}"
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
