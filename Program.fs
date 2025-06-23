open System
open System.Collections.Generic

open visual
open maps
open fields

let mappath = "./map1.txt"

let gameOver (map: char[,])  (s: string) ( n : int ) (m : int) =
    printMap map n m
    printfn "%s" s
    Environment.Exit(3)

let getCoord ( s : string ) ( max : int ) : int =
    printf "- %s: " s
    match Int32.TryParse(Console.ReadLine()) with
        | (true, v) -> if not (v > max) then v
                        else failwithf "coordinate for %s may not be more than %A" s max
        | (false, _) -> failwithf "coordinate must be int"


// inits
let secretmap : char[,] = fileToChars(mappath)
let n, m = (secretmap |> Array2D.length1), (secretmap |> Array2D.length2)
let shownmap : char[,] = Array2D.create n m hidechar//secretmap |> Array2D.map (fun x -> hidechar)
let fieldmap : field[,] = getFieldMap secretmap n m
//updateNumsMap fieldmap
let exposedmap : char[,] = fieldmap |> Array2D.map(fieldToChar)
let visited = HashSet<int * int>()
let mutable remainingfields = (n * m) - numbombs
// game loop
while remainingfields > 0 do
    printMap shownmap n m
    let r = getCoord "X" (n - 1)
    let c = getCoord "Y" (m - 1)
    printfn "%A %A" r c
    match fieldmap.[r,c] with
            | Safe 0 -> clearUp fieldmap exposedmap shownmap r c n m visited
            | Safe x -> remainingfields <- remainingfields - 1
                        shownmap.[r,c] <- exposedmap.[r,c]//clearUp fieldmap secretmap exposedMap r c n m
            | Bomb -> gameOver exposedmap "Game Lost!" n m

gameOver exposedmap "Game Won!" n m
