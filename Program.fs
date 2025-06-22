open System
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
    let r = getCoord "X" (n - 1)
    let c = getCoord "Y" (m - 1)
    printfn "%A %A" r c
    match fieldmap.[r,c] with
            | Safe x -> remainingfields <- remainingfields - 1
                        shownmap.[r,c] <- exposedMap.[r,c]//clearUp fieldmap secretmap exposedMap r c n m
            | Bomb -> gameOver exposedMap "Game Lost!" n m

gameOver exposedMap "Game Won!" n m
