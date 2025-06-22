module visual
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