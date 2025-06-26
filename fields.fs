module fields

let hidechar =  '□'
let blankchar = '.'
let bombchar = 'x'

type field =
    | Bomb
    | Safe of int

let fieldToChar ( f : field ) : char =
    match f with
        | Bomb -> bombchar
        | Safe 0 -> blankchar
        | Safe x -> char (x + int '0')
