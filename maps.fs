module maps

open System.Collections.Generic
open System
open fields

let incrementSafe ( map : field[,] ) ( r : int ) ( c : int ) =
    match map.[r,c] with
         | Bomb -> ()
         | Safe x -> map.[r,c] <- Safe (x + 1)

let incrementSafeVals ( map : field[,] )
                      ( n : int ) ( m : int )
                      ( r : int ) ( c : int ) =
    if r > 0 then
        incrementSafe map (r-1) c // upper middle
        if c > 0 then incrementSafe map (r-1) (c-1) // upper left corner
        if c < (m-1) then incrementSafe map (r-1) (c+1) // upper right corner
    if r < (n-1) then
        incrementSafe map (r+1) c // lower middle
        if c > 0 then incrementSafe map (r+1) (c-1) // lower left corner
        if c < (m-1) then incrementSafe map (r+1) (c+1) // lower right corner
    if c > 0 then incrementSafe map r (c-1) // left middle
    if c < (m-1) then incrementSafe map r (c+1) // right middle

let updateNumsMap( fmap : field[,] ) ( n : int ) ( m : int ) =
    fmap |> Array2D.iteri (fun r c v -> if v = Bomb then incrementSafeVals fmap n m r c)

let randomMap ( bombs : int ) ( n : int ) ( m : int ) ( r : int ) ( c : int ) : field[,] =
    let map = Array2D.create n m (Safe 0)
    let rnd = new Random()
    let mutable togo = bombs
    while togo > 0 do
        let randRow, randCol = rnd.Next(n), rnd.Next(m) // gives non-negative int, upper bound exclusive
        if (not (map.[randRow,randCol] = Bomb)) && (not (randRow = r && randCol = c)) then
            map.[randRow,randCol] <- Bomb
            togo <- togo - 1
    updateNumsMap map n m
    map

let rec clearUp ( fieldmap : field[,] ) ( secret : char[,] ) ( exposed : char[,])
                ( r : int ) ( c : int )
                ( n : int ) ( m : int )
                ( visited : HashSet<int * int> )
                ( counter : byref<int> ) =
    if r < 0 || r >= n || c < 0 || c >= m then ()
    elif visited.Contains (r, c) then ()
    else
        visited.Add (r, c) |> ignore
        if not (fieldmap.[r,c] = Bomb) then
            exposed.[r,c] <- secret.[r,c]
            counter <- counter - 1
        match fieldmap.[r,c] with
             | Safe 0 ->
                 for rr in -1..1 do
                     for cc in -1..1 do
                         clearUp fieldmap secret exposed (r+rr) (c+cc) n m visited &counter
             | _ -> ()
