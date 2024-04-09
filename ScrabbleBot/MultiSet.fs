module MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> 

    let empty = R Map.empty

    let isEmpty  (R s : MultiSet<'a>) =   Map.isEmpty s

    let size (R s : MultiSet<'a>) =
        if Map.isEmpty s then
            0u
        else
            (Map.fold (fun acc a u ->( acc + u)) 0u s)
    
    let contains (key : 'a) (R s : MultiSet<'a>) = Map.containsKey key s

    let numItems (key : 'a) (R s : MultiSet<'a>) =
        if Map.containsKey key s then
            s.[key]
        else
            0u

    let add (key : 'a) (un : uint32) (R s : MultiSet<'a>) : MultiSet<'a> =
        if Map.containsKey key s then
            let currentVal = s.[key]
            R (Map.add key (currentVal + un) s)
        else
            R (Map.add key un s)
        

    let addSingle (key : 'a) (R s : MultiSet<'a>) : MultiSet<'a> =
        if Map.containsKey key s then
            let currentVal = s.[key]
            R (Map.add key (currentVal + 1u) s)
        else
            R (Map.add key 1u s)
    
    let remove (key : 'a) (un : uint32) (R s  : MultiSet<'a>) : MultiSet<'a> =
            if Map.containsKey key s then
               let currentVal = s.[key]
               if currentVal - un > currentVal then
                   R(Map.remove key s)
               else
                   R (Map.add key (currentVal - un) s)
            else
               R s

    let removeSingle (key : 'a) (R s : MultiSet<'a>) : MultiSet<'a> =
            if Map.containsKey key s then
               let currentVal = s.[key]
               if currentVal - 1u > currentVal then
                   R(Map.remove key s)
               else
                   R (Map.add key (currentVal - 1u) s)
            else
                R s


    let fold (F : 'b -> 'a -> uint32 -> 'b) (acc : 'b) (R s : MultiSet<'a>) =
        Map.fold F acc s
    let foldBack (F : 'a -> uint32 -> 'b -> 'b) (R s : MultiSet<'a>) (acc : 'b) =
        Map.foldBack F s acc
    
    let ofList (_ : 'a list) : MultiSet<'a> =  empty
    let toList (_ : MultiSet<'a>) : 'a list = []


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = empty

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
       
    