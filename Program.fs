open System

let mappath = "./map1.txt"
let hidechar =  '□'
let nothingchar = '.'
let bombchar = 'x'
let mutable numbombs = 0

type field =
    | Bomb
    | Safe of int

let charToField (s : char) : field =
    match s with
        | '.' -> Safe 0
        | '*' -> numbombs <- numbombs+1; Bomb
        | _ -> failwithf "unknown charachter encountered when reading map: %A" s

let fieldToChar (f : field) : char =
    match f with
        | Bomb -> bombchar
        | Safe 0 -> nothingchar
        | Safe x -> char (x + int '0')

let fileToChars (path : string) : char[,] =
    array2D(IO.File.ReadLines(path) |> Seq.toArray |> Seq.map(fun x -> x.ToCharArray()))

let incrementSafe (map : field[,]) (r : int) (c : int) =
    let cur = map.[r,c]
    if not (cur = Bomb) then
        let s =
            match cur with
            | Safe x -> Safe (x+1)
        map.[r,c] <- s

let updateSafes (map : field[,]) (n : int) (m : int) (r : int) (c : int) =
    if r > 0 then // above, middle
        incrementSafe map (r-1) c // upper middle
        if c > 0 then incrementSafe map (r-1) (c-1) // upper left corner
        if c < n-1 then incrementSafe map (r-1) (c+1) // upper right corner
    if r < n-1 then // below, middle
        incrementSafe map (r+1) c // lower middle
        if c > 0 then incrementSafe map (r+1) (c-1) // lower left corner
        if c < n-1 then incrementSafe map (r+1) (c+1) // lower right corner
    if c > 0 then incrementSafe map r (c-1) // left middle
    if c < n-1 then incrementSafe map r (c+1) // right middle

let updateNumsMap(originalMap : field[,]) =
    let n = originalMap |> Array2D.length1
    let m = originalMap |> Array2D.length2
    for r in 0..n-1 do
        for c in 0..m-1 do
            if originalMap.[r,c] = Bomb then
                updateSafes originalMap n m r c

let getFieldMap (lines : char[,]) (n : int) (m : int) : field[,] =
    let newlines = lines |> Array2D.map charToField
    updateNumsMap newlines
    newlines

let printMap (map: char[,]) (rows : int) (cols : int) =
    // Print column headers
    printf "    "
    for c in 0 .. cols-1 do
        printf "%2d " c
    printfn ""

    // Top border
    printf "   ┌"
    for _ in 0 .. cols-1 do
        printf "───"
    printfn "┐"

    // Print each row
    for r in 0 .. rows - 1 do
        printf "%2d │" r
        for c in 0 .. cols - 1 do
            printf " %c " map.[r, c]
        printfn "│"

    // Bottom border
    printf "   └"
    for _ in 0 .. cols-1 do
        printf "───"
    printfn "┘"

let gameOver (map: char[,])  (s: string) ( n : int ) (m : int) =
    printMap map n m
    printfn "%s" s
    Environment.Exit(3)

let getCoord ( s : string ) ( max : int ) : int =
    printf "- %s: " s
    match Int32.TryParse(Console.ReadLine()) with
        | (true, v) -> if not (v > max-1) then v
                        else failwithf "coordinate for %s may not be more than %A" s max-1
        | (false, _) -> failwithf "coordinate must be int"

// let rec clearUp ( fieldmap : field[,] ) ( secret : char[,] ) ( exposed : char[,]) ( r : int ) ( c : int ) ( n : int ) ( m : int ) =
//     if not (fieldmap.[r,c] = Bomb) then
//         exposed.[r,c] <- secret.[r,c]
//         // clear up middle above
//         if r > 0 then clearUp fieldmap secret exposed (r-1) c n m
//         // clear up middle below
//         if r < n-1 then clearUp fieldmap secret exposed (r+1) c n m
//         // clean up right middle
//         if c < m-1 then clearUp fieldmap secret exposed r (c+1) n m
//         // clean up left middle
//         if c > 0 then clearUp fieldmap secret exposed r (c-1) n m

// inits
let secretmap : char[,] = fileToChars(mappath)
let n, m = (secretmap |> Array2D.length1), (secretmap |> Array2D.length2)
let shownmap : char[,] = Array2D.create n m hidechar//secretmap |> Array2D.map (fun x -> hidechar)
let fieldmap : field[,] = getFieldMap secretmap n m
//updateNumsMap fieldmap
let exposedMap : char[,] = fieldmap |> Array2D.map(fieldToChar)
let mutable remainingfields = (n * m) - numbombs
// game loop
while remainingfields > 0 do
    printMap shownmap n m
    let r = getCoord "X" n
    let c = getCoord "Y" m
    printfn "%A %A" r c
    match fieldmap.[r,c] with
            | Safe x -> remainingfields <- remainingfields - 1
                        shownmap.[r,c] <- exposedMap.[r,c]//clearUp fieldmap secretmap exposedMap r c n m
            | Bomb -> gameOver exposedMap "Game Lost!" n m

gameOver exposedMap "Game Won!" n m
