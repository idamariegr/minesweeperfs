module maps

open System.Collections.Generic
open System
open fields

let fileToChars ( path : string ) : char[,] =
    array2D(IO.File.ReadLines(path) |> Seq.toArray |> Seq.map(fun x -> x.ToCharArray()))

let incrementSafe (map : field[,]) (r : int) (c : int) =
    let cur = map.[r,c]
    match cur with
         | Bomb -> ()
         | Safe x -> map.[r,c] <- Safe (x + 1)

let updateSafe (map : field[,]) (n : int) (m : int) (r : int) (c : int) =
    if r > 0 then // above, middle
        incrementSafe map (r-1) c // upper middle
        if c > 0 then incrementSafe map (r-1) (c-1) // upper left corner
        if c < n-1 then incrementSafe map (r-1) (c+1) // upper right corner
    if r < n-1 then // below, middle
        incrementSafe map (r+1) c // lower middle
        if c > 0 then incrementSafe map (r+1) (c-1) // lower left corner
        if c < m-1 then incrementSafe map (r+1) (c+1) // lower right corner
    if c > 0 then incrementSafe map r (c-1) // left middle
    if c < m-1 then incrementSafe map r (c+1) // right middle

let updateNumsMap(originalMap : field[,]) ( n : int ) ( m : int ) =
    for r in 0..n-1 do
        for c in 0..m-1 do
            if originalMap.[r,c] = Bomb then
                updateSafe originalMap n m r c

let randomMap ( bombs : int ) ( n : int ) ( m : int ) : field[,] =
    let map = Array2D.create n m (Safe 0)
    let rnd = new Random()
    let mutable togo = bombs
    while togo > 0 do
        let randRow = rnd.Next(0,n) // lower bound inclusive, upper bound exclusive
        let randCol = rnd.Next(0,m)
        if not (map.[randRow,randCol] = Bomb) then
            map.[randRow,randCol] <- Bomb
            togo <- togo - 1
    updateNumsMap map n m
    map

let getFieldMap (lines : char[,]) (n : int) (m : int) : field[,] =
    let newlines = lines |> Array2D.map charToField
    updateNumsMap newlines n m
    newlines

let rec clearUp ( fieldmap : field[,] ) ( secret : char[,] ) ( exposed : char[,])
                ( r : int ) ( c : int )
                ( n : int ) ( m : int )
                ( visited : HashSet<int * int> )
                ( counter : byref<int> ) =
    if r < 0 || r >= n || c < 0 || c >= m then ()
    elif visited.Contains (r, c) then ()
    else
        visited.Add (r, c) |> ignore
        exposed.[r,c] <- secret.[r,c]
        counter <- counter - 1
        match fieldmap.[r,c] with
             | Safe 0 ->
                 for rr in -1..1 do
                     for cc in -1..1 do
                         clearUp fieldmap secret exposed (r+rr) (c+cc) n m visited &counter
             | _ -> ()

let decrementSafe ( map : field[,] ) ( r : int ) ( c : int ) ( cnt : byref<int> ) =
    match map.[r,c] with
        | Bomb -> cnt <- cnt + 1
        | Safe x -> if x > 0 then map.[r,c] <- Safe (x-1) else ()

let removeBomb ( map : field[,] ) ( r : int ) ( c : int ) ( n : int ) ( m : int ) ( cnt : byref<int> ) =
    if not (map.[r,c] = Bomb) then ()
    let mutable counter = 0
    map.[r,c] <- Safe 0
    cnt <- cnt - 1
    if r > 0 then
        decrementSafe map (r-1) c &counter
        if c > 0 then decrementSafe map (r-1) (c-1) &counter
        if c < (n-1) then decrementSafe map (r-1) (c+1) &counter
    if r < (n-1) then
        decrementSafe map (r+1) c &counter
        if r > 0 then decrementSafe map (r+1) (c-1) &counter
        if r < (m-1) then decrementSafe map (r+1) (c+1) &counter
    if c > 0 then decrementSafe map r (c-1) &counter
    if c < (m+1) then decrementSafe map r (c+1) &counter
    map.[r,c] <- Safe counter
