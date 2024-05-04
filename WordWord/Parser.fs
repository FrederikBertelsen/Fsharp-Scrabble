module Parser

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


let pIntToChar = pstring "intToChar" <?> "intToChar"
let pCharToInt = pstring "charToInt" <?> "charToInt"
let pPointValue = pstring "pointValue" <?> "pointValue"

let pToUpper = pstring "toUpper" <?> "toUpper"
let pToLower = pstring "toLower" <?> "toLower"
let pCharValue = pstring "charValue" <?> "charValue"

let pTrue = pstring "true" <?> "true"
let pFalse = pstring "false" <?> "false"
let pIsDigit = pstring "isDigit" <?> "isDigit"
let pIsLetter = pstring "isLetter" <?> "isLetter"
let pIsVowel = pstring "isVowel" <?> "isVowel"

let pif = pstring "if" <?> "if"
let pthen = pstring "then" <?> "then"
let pelse = pstring "else" <?> "else"
let pwhile = pstring "while" <?> "while"
let pdo = pstring "do" <?> "do"
let pdeclare = pstring "declare" <?> "declare"

let whitespaceChar: Parser<char> = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let pletter: Parser<char> = satisfy System.Char.IsLetter <?> "letter"
let palphanumeric: Parser<char> = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

let spaces: Parser<char list> = many whitespaceChar <?> "space"
let spaces1: Parser<char list> = many1 whitespaceChar <?> "space1"

let (.>*>.) (ps1: Parser<'a>) (ps2: Parser<'b>) : Parser<'a * 'b> = ps1 .>> spaces .>>. ps2
let (.>*>) ps1 ps2 : Parser<'a> = ps1 .>> spaces .>> ps2
let (>*>.) ps1 ps2 : Parser<'b> = ps1 .>> spaces >>. ps2

let parenthesise ps : Parser<'a> = pchar '(' >*>. ps .>*> pchar ')' <?> "parenthesise"

let pid: Parser<string> = (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_') |>> (fun (x, xs) -> System.String((x :: xs) |> List.toArray))  <?> "Identifier"
let unop (op: Parser<'a>) (a: Parser<'b>) : Parser<'b> = op >*>. a
let binop (op: Parser<'a>) (a: Parser<'b>) (b: Parser<'c>) : Parser<'b * 'c> = a .>*> op .>*>. b

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()
let CexpParse, cref = createParserForwardedToRef<cExp> ()

let BexpParse, bref = createParserForwardedToRef<bExp>()
let BexpParse2, bref2 = createParserForwardedToRef<bExp>()
let BexpParse3, bref3 = createParserForwardedToRef<bExp>()

let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
do tref := choice [ AddParse; SubParse; ProdParse ]


let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
do pref := choice [ MulParse; DivParse; ModParse; AtomParse ]


let NumParse = pint32 |>> N <?> "Int"
let ParenParse = (parenthesise TermParse) <?> "Parenthesise Int"
let NegParse = unop (pchar '-') AtomParse |>> (fun a -> Mul(N(-1), a)) <?> "Neg"
let PointValueParse = unop pPointValue (parenthesise TermParse) |>> PV <?> "PointValue"
let VariableParse = pid |>> V <?> "Variable"
let CharToIntParse = unop pCharToInt (parenthesise CexpParse) |>> CharToInt <?> "CharToInt"
do aref := choice [NegParse; NumParse; PointValueParse; CharToIntParse; ParenParse; VariableParse]
let AexpParse = TermParse


let CharParse = between (pchar '\'') (pchar '\'') anyChar |>> C <?> "Char"
let parenChar = (parenthesise CexpParse) <?> "Parenthesise Char"
let pCVL = unop pCharValue (parenthesise AexpParse) |>> CV <?> "charValue"
let ToUpper = unop pToUpper (parenthesise CexpParse) |>> ToUpper <?> "toUpper"
let ToLower = unop pToLower (parenthesise CexpParse) |>> ToLower <?> "toLower"
let IntToChar = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "intToChar"
do cref := choice [CharParse; pCVL; IntToChar; ToUpper; ToLower; parenChar]


let pAnd = binop (pstring "/\\") BexpParse BexpParse |>> Conj <?> "Conj"
let pOr = binop (pstring "\\/") BexpParse BexpParse |>> (fun (a, b) -> Not(Conj(Not a, Not b))) <?> "Disj"
do bref := choice [pAnd; pOr; BexpParse2]


let pEq = binop (pstring "=") AexpParse AexpParse |>> AEq <?> "Equal"
let pNeq = binop (pstring "<>") AexpParse AexpParse |>> (fun (a, b) -> Not(AEq(a, b))) <?> "Not Equal"
let pLt = binop (pstring "<") AexpParse AexpParse |>> ALt <?> "Less Than"
let pLe = binop (pstring "<=") AexpParse AexpParse |>> (fun (a, b) -> Not(ALt(b, a))) <?> "Less Than or Equal"
let pGt = binop (pstring ">") AexpParse AexpParse |>> (fun (a, b) -> ALt(b, a)) <?> "Greater Than"
let pGe = binop (pstring ">=") AexpParse AexpParse |>> (fun (a, b) -> Not(ALt(a, b))) <?> "Greater Than or Equal"
do bref2 := choice [pEq; pNeq; pLt; pLe; pGt; pGe; BexpParse3]


let parentBool = parenthesise BexpParse <?> "Parenthesise Bool"
let True = pTrue |>> (fun _ -> TT)  <?> "True"
let False = pFalse |>> (fun _ -> FF)  <?> "False"
let pNot = pstring "~" >*>. BexpParse |>> Not <?> "Not"
let IsVowel = unop pIsVowel (parenthesise CharParse) |>> IsVowel <?> "IsVowel"
do bref3 := choice [ True; False; pNot; IsVowel; parentBool]

let stmntParse = pstring "not implemented"

let parseSquareProg _ = failwith "not implemented"

let parseBoardProg _ = failwith "not implemented"

let mkBoard : boardProg ->
    board = fun b -> {
        center = b.center
        defaultSquare = Map.empty
        squares = fun _ -> Success (Some Map.empty)
    }
