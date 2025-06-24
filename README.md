# Minesweeper
console game with functional programming!

run the game from the root directory with `dotnet run`

## workings
- generates random map (of abstract field-type elements)
- makes char maps to print from the field-map
    - all maps are of type [Array2D](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-array2dmodule.html)
- ensures first given coordinates do not result in game over
    - if first chosen field is a bomb, then it is changed to a safe field. the values of the safe fields around it are decremented.
- if safe field with value 0 is ever chosen, then it and all of it's neighbours are cleared recursively
- keep taking input and updating the map until only bombs remain

## todo
- [ ] allow for one to regret a written row-index
