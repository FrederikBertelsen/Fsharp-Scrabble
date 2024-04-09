module Eval

    open StateMonad
    open ScrabbleUtil

    (* Code for testing *)

    let hello = [('H',4);('E',1);('L',1);('L',1);('O',1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b =
        a >>= fun a' ->
        b >>= fun b' ->
            ret (a' + b')    
    let div a b =
        a >>= fun a' ->
        b >>= fun b' ->
            if b' <> 0 then
                ret (a' / b')
            else
                fail DivisionByZero

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval (exp : aExp) : SM<int> =
        match exp with
        | N n -> ret n
        | V x -> lookup x
        | WL -> wordLength
        | PV pos ->
            arithEval pos >>= fun p ->
            pointValue p
            
        | Add (aexp1, aexp2) ->
            arithEval aexp1 >>= fun x ->
            arithEval aexp2 >>= fun y ->
            add (ret x) (ret y)
        
        | Sub (aexp1, aexp2) ->
            arithEval aexp1 >>= fun x ->
            arithEval aexp2 >>= fun y ->
            ret (x - y)
        | Mul (aexp1, aexp2) ->
            arithEval aexp1 >>= fun x ->
            arithEval aexp2 >>= fun y ->
            ret (x * y)
        | Div (aexp1, aexp2) ->
            arithEval aexp1 >>= fun x ->
            arithEval aexp2 >>= fun y ->
            div (ret x) (ret y)
                
        | Mod (aexp1, aexp2) ->
            arithEval aexp1 >>= fun x ->
            arithEval aexp2 >>= fun y ->
            if y = 0 then
                fail DivisionByZero // added this since it did not follow from the div function
            else
                ret (x % y)
        | CharToInt (CV ae) ->
            arithEval ae >>= fun pos ->
            characterValue pos >>= fun c ->
            ret (int c)

    let rec  charEval (exp : cExp) : SM<char> =
        match exp with
        | C c -> ret c
        | CV aexp -> arithEval aexp >>= fun pos -> characterValue pos
        | ToUpper cexp ->
            charEval cexp >>= fun c ->
            ret (System.Char.ToUpper c)
        | ToLower cexp ->
            charEval cexp >>= fun c ->
            ret (System.Char.ToLower c)
        | IntToChar aexp ->
            arithEval aexp >>= fun x ->
            ret (System.Char.ConvertFromUtf32 x).[0]     

    let rec boolEval (exp : bExp) : SM<bool> =
        match exp with
        | TT -> ret true
        | FF -> ret false
        | AEq (aexp1, aexp2) ->
            arithEval aexp1 >>= fun x ->
            arithEval aexp2 >>= fun y ->
            ret (x = y)
        | ALt (aexp1, aexp2) ->
            arithEval aexp1 >>= fun x ->
            arithEval aexp2 >>= fun y ->
            ret (x < y)
        | Not bexp ->
            boolEval bexp >>= fun b ->
            ret (not b)
        | Conj (bexp1, bexp2) ->
            boolEval bexp1 >>= fun x ->
            boolEval bexp2 >>= fun y ->
            ret (x && y)
        | IsLetter cexp ->
            charEval cexp >>= fun c ->
            ret (System.Char.IsLetter c)
        | IsDigit cexp ->
            charEval cexp >>= fun c ->
            ret (System.Char.IsDigit c)
        | IsVowel cxpe ->
            charEval cxpe >>= fun c' ->
            let cLower = System.Char.ToLower c'
            if cLower = 'a' || cLower = 'e' || cLower = 'i' || cLower = 'o' || cLower = 'u' then
                ret true
            else
                ret false
        
        
    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c x boardStmnt ids = failwith "Not implemented"
    