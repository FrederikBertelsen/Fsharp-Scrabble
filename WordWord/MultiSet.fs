module MultiSet

type MultiSet<'a when 'a: comparison> = R of Map<'a, uint32>

let empty = R Map.empty

let isEmpty (R s: MultiSet<'a>) = Map.isEmpty s

let size (R s: MultiSet<'a>) =
    if Map.isEmpty s then
        0u
    else
        (Map.fold (fun acc a u -> (acc + u)) 0u s)

let contains (key: 'a) (R s: MultiSet<'a>) = Map.containsKey key s

let numItems (key: 'a) (R s: MultiSet<'a>) =
    if Map.containsKey key s then s.[key] else 0u

let add (e: 'a) (n: uint32) (R m: MultiSet<'a>) : MultiSet<'a> =
    R(Map.add e (if m.ContainsKey e then (Map.find e m) + n else n) m)

let addSingle (e: 'a) (m: MultiSet<'a>) : MultiSet<'a> = add e 1u m

let remove (e: 'a) (n: uint32) (R m: MultiSet<'a>) : MultiSet<'a> =
    let o = Option.defaultValue 0u (Map.tryFind e m)

    if m.ContainsKey e then
        if o > n then R(Map.add e (o - n) m) else R(Map.remove e m)
    else
        R m

let removeSingle (e: 'a) (m: MultiSet<'a>) : MultiSet<'a> = remove e 1u m


let fold (F: 'b -> 'a -> uint32 -> 'b) (acc: 'b) (R s: MultiSet<'a>) = Map.fold F acc s
let foldBack (F: 'a -> uint32 -> 'b -> 'b) (R s: MultiSet<'a>) (acc: 'b) = Map.foldBack F s acc

let ofList (_: 'a list) : MultiSet<'a> = empty
let toList (R multiSet: MultiSet<'a>) : 'a list =
    Map.fold (fun accumulatedList key value -> accumulatedList @ List.replicate (int value) key) [] multiSet

let map (_: 'a -> 'b) (_: MultiSet<'a>) : MultiSet<'b> = empty

let union (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = empty
let sum (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = empty

let subtract (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = empty

let intersection (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = empty
