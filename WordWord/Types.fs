module Types

    open StateMonad
    open ScrabbleUtil
    
    type word   = (char * int) list
    
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun = coord -> Result<square option, Error>
    
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }