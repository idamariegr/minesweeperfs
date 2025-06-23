open System
open System.Collections.Generic

open visual
open maps
open fields

let mappath = "Assets/map1.txt"

let gameOver (map: char[,])  (s: string) ( n : int ) ( m : int ) =
    printMap map n m
    printfn "%s" s
    Environment.Exit(3)

let getCoord ( s : string ) ( max : int ) : int =
    printf "- %s: " s
    match Int32.TryParse(Console.ReadLine()) with
        | (true, v) -> if not (v > max) then v
                        else failwithf "coordinate for %s may not be more than %A" s max
        | (false, _) -> failwithf "coordinate must be int"

let gameRandom ( bombs : int ) ( n : int ) ( m : int ) =
    let fieldmap = randomMap bombs n m
    let shownmap : char[,] = Array2D.create n m hidechar
    let secretmap = fieldmap |> Array2D.map(fieldToChar)
    let visited = HashSet<int * int>()
    let mutable safeFieldsToGo = (n * m) - bombs
    while safeFieldsToGo > 0 do
        printMap shownmap n m
        let r = getCoord "Row" (n - 1)
        let c = getCoord "Column" (m - 1)
        match fieldmap.[r,c] with
            | Safe 0 -> clearUp fieldmap secretmap shownmap r c n m visited &safeFieldsToGo
            | Safe x -> safeFieldsToGo <- safeFieldsToGo - 1
                        shownmap.[r,c] <- secretmap.[r,c]
            | Bomb -> gameOver secretmap "Game Lost!" n m

    gameOver secretmap "Game Won!" n m

let gameFromPath ( path : string) =
    let initialmap : char[,] = fileToChars(path)
    let n, m = (initialmap |> Array2D.length1), (initialmap |> Array2D.length2)
    let shownmap : char[,] = Array2D.create n m hidechar
    let fieldmap : field[,] = getFieldMap initialmap n m
    let secretmap : char[,] = fieldmap |> Array2D.map(fieldToChar)
    let visited = HashSet<int * int>()
    let mutable numbombs = 0
    fieldmap |> Array2D.iter(fun f -> if f = Bomb then numbombs <- numbombs + 1)
    let mutable safeFieldsToGo = (n * m) - numbombs
    // game loop
    while safeFieldsToGo > 0 do//remainingfields > 0 do
        printMap shownmap n m
        let r = getCoord "Row" (n - 1)
        let c = getCoord "Column" (m - 1)
        match fieldmap.[r,c] with
            | Safe 0 -> clearUp fieldmap secretmap shownmap r c n m visited &safeFieldsToGo
            | Safe x -> safeFieldsToGo <- safeFieldsToGo - 1
                        shownmap.[r,c] <- secretmap.[r,c]
            | Bomb -> gameOver secretmap "Game Lost!" n m

    gameOver secretmap "Game Won!" n m

// running the game
//gameFromPath(mappath)
gameRandom 35 15 15
