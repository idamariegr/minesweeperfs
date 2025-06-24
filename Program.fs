open System
open System.Collections.Generic

open visual
open maps
open fields

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
    let shownmap = Array2D.create n m hidechar
    let visited = HashSet<int * int>()
    let mutable safeFieldsToGo = (n * m) - bombs

    //getting input for the first time
    printMap shownmap n m
    let r = getCoord "Row" (n - 1)
    let c = getCoord "Column" (m - 1)
    removeBomb fieldmap r c n m &safeFieldsToGo// handles if first chosen coord is bomb
    let secretmap = fieldmap |> Array2D.map(fieldToChar)
    clearUp fieldmap secretmap shownmap r c n m visited &safeFieldsToGo

    // game loop
    while safeFieldsToGo > 0 do
        printMap shownmap n m
        let r = getCoord "Row" (n - 1)
        let c = getCoord "Column" (m - 1)
        match fieldmap.[r,c] with
            | Safe x -> clearUp fieldmap secretmap shownmap r c n m visited &safeFieldsToGo
            | Bomb -> gameOver secretmap "Game Lost!" n m

    gameOver secretmap "Game Won!" n m

// running the game
gameRandom 90 15 15
