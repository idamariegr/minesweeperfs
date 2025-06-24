module visual
open System

let printMap (map: char[,]) (rows : int) (cols : int) =
    // column numbers
    printf "    "
    for i in 0 .. cols-1 do
        printf "%2d " i
    printfn ""

    // top border
    printf "   ┌"
    for _ in 0 .. cols-1 do
        printf "───"
    printfn "┐"

    // the actual map
    for r in 0 .. rows - 1 do
        printf "%2d │" r
        for c in 0 .. cols - 1 do
            printf " %c " map.[r, c]
        printfn "│"

    // bottom border
    printf "   └"
    for _ in 0 .. cols-1 do
        printf "───"
    printfn "┘"
