﻿module Parser

open Eval
open ScrabbleUtil
open StateMonad

(*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

// open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
open FParsecLight.TextParser // Industrial parser-combinator library. Use for Scrabble Project.

type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>
type square = Map<int, squareFun>

type boardFun2 = coord -> Result<square option, Error>

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun2 }


let pIntToChar = pstring "intToChar"
let pPointValue = pstring "pointValue"

let pCharToInt = pstring "charToInt"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"
let pIsVowel = pstring "isVowel"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

let whitespaceChar: Parser<char> = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let pletter: Parser<char> = satisfy System.Char.IsLetter <?> "letter"

let palphanumeric: Parser<char> =
    satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

let spaces: Parser<char list> = many whitespaceChar <?> "space"
let spaces1: Parser<char list> = many1 whitespaceChar <?> "space1"

let (.>*>.) (ps1: Parser<'a>) (ps2: Parser<'b>) : Parser<'a * 'b> = ps1 .>> spaces .>>. ps2

let (.>*>) ps1 ps2 : Parser<'a> = ps1 .>> spaces .>> ps2
let (>*>.) ps1 ps2 : Parser<'b> = ps1 .>> spaces >>. ps2

let parenthesise ps : Parser<'a> = pchar '(' >*>. ps .>*> pchar ')'

let pid: Parser<string> =
    (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_')
    |>> fun (x, xs) -> System.String((x :: xs) |> List.toArray)


let unop (op: Parser<'a>) (a: Parser<'b>) : Parser<'b> = op >*>. a
let binop (op: Parser<'a>) (a: Parser<'b>) (b: Parser<'c>) : Parser<'b * 'c> = a .>*> op .>*>. b

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()

let BexpParse, bref = createParserForwardedToRef<bExp> ()
let CexpParse, cref = createParserForwardedToRef<cExp> ()


let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
do tref := choice [ AddParse; SubParse; ProdParse ]

let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
do pref := choice [ MulParse; DivParse; ModParse; AtomParse ]

let NumParse = pint32 |>> N <?> "Int"
let ParenParse = parenthesise TermParse
let NegParse = unop (pchar '-') AtomParse |>> (fun a -> Mul(N(-1), a)) <?> "Neg"
// let PValueParse = between (pstring "pointValue (") (pchar ')') TermParse |>> PV <?> "PointValue"
let PValueParse =
    unop (pstring "pointValue") (parenthesise TermParse) |>> PV <?> "PointValue"

let VariableParse = pid |>> V <?> "Variable"

let CharToIntParse =
    unop (pstring "charToInt") (parenthesise CexpParse) |>> CharToInt
    <?> "CharToInt"

do
    aref
    := choice [ CharToIntParse; NegParse; PValueParse; VariableParse; ParenParse; NumParse ]

let AexpParse = TermParse
let CharParse = between (pchar '\'') (pchar '\'') anyChar |>> C

let pCVL =
    unop (pstring "charValue") (parenthesise AexpParse) |>> CV <?> "charValue"

let ToUpper =
    unop (pstring "toUpper") (parenthesise CexpParse) |>> ToUpper <?> "toUpper"

let ToLower =
    unop (pstring "toLower") (parenthesise CexpParse) |>> ToLower <?> "toLower"

let IntToChar =
    unop (pstring "intToChar") (parenthesise AexpParse) |>> IntToChar
    <?> "intToChar"

do cref := choice [ CharParse; ToLower; ToUpper; IntToChar; pCVL ]


let pTrue = pstring "true" |>> fun _ -> TT
let pFalse = pstring "false" |>> fun _ -> FF
let pNot = pstring "~" >*>. BexpParse |>> Not

let pAnd = binop (pstring "/\\") BexpParse BexpParse |>> Conj

let pOr = binop (pstring "\\/") BexpParse BexpParse |>> fun (a, b) -> Not(Conj(Not a, Not b))

let pEq = binop (pstring "=") AexpParse AexpParse |>> AEq
let pNeq = binop (pstring "<>") AexpParse AexpParse |>> fun (a, b) -> Not(AEq(a, b))
let pLt = binop (pstring "<") AexpParse AexpParse |>> ALt
let pLe = binop (pstring "<=") AexpParse AexpParse |>> fun (a, b) -> Not(ALt(b, a))
let pGt = binop (pstring ">") AexpParse AexpParse |>> fun (a, b) -> ALt(b, a)
let pGe = binop (pstring ">=") AexpParse AexpParse |>> fun (a, b) -> Not(ALt(a, b))

do bref := choice [ pTrue; pFalse; pNot; pAnd; pOr; pEq; pNeq; pLt; pLe; pGt; pGe ]

let stmntParse = pstring "not implemented"


let parseSquareProg _ = failwith "not implemented"

let parseBoardProg _ = failwith "not implemented"

let parseSquareFun (sp: squareProg) : square =
    // Implement the logic to convert a squareProg to a square
    // This is a placeholder. You need to replace it with your actual implementation
    Map.empty

let parseBoardFun (bp: string) (squares: Map<int, square>) : boardFun2 =
    // Implement the logic to convert a boardProg and a map of squares to a boardFun2
    // This is a placeholder. You need to replace it with your actual implementation
    fun (coord: coord) -> Result<square option, Error>


// let mkBoard (bp : boardProg) : board = failwith "not implemented"
let mkBoard (bp: boardProg) : board =
    // Parse all the square programs in the squares field of the boardProg object
    let parsedSquares = Map.map (fun _ sp -> parseSquareFun sp) bp.squares

    // Set the center field to the center field of the boardProg object
    let center = bp.center

    // Set the defaultSquare field to the square function with key x in the parsedSquares map
    let defaultSquare = Map.find bp.usedSquare parsedSquares

    // Set the squares field to the result of parsing the board program with the parsedSquares map
    let squares = parseBoardFun bp.prog parsedSquares

    // Create a new board with the converted fields
    { center = center
      defaultSquare = defaultSquare
      squares = squares }
