module fields

//let mappath = "./map1.txt"
let hidechar =  '□'
let blankchar = '.'
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
        | Safe 0 -> blankchar
        | Safe x -> char (x + int '0')
