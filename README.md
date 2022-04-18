# haskell-jumble

`formatGrid` uses the `unlines` function from the std lib to output the data in a grid.

`getLines` is the function we use to search in all directions.

Searches left to right (from beginning to end of list of strings `grid :: [String]`)
`horizontal` = `grid`

Transposes rows and columns in order to search from top to bottom.
`vertical` = `transpose grid`

`lines` is the variable were we will include our list of lists i.e. our `grid` in normal order, _reverse_ order, _transposed_ order and _schewed_ order in order to search left to right and right to left, top to bottom and bottom to top and diagonally.

`findWord` searches every line of the `grid` for a given `String` and returns a `Maybe String`. This way, we can create a list of `Maybe` values, filtering out all words that don't match.

## `skew`

In order to `skew` the `grid`, we will again use the `transpose` function. The idea is to recursively `prepend` a `'_'` character to every line _except_ the first line

`skew` takes a variable type `Grid` and returns a var type `Grid`
When writing a recursive algorithm you must always remember to include the **base** case
`skew [] = []`
Our 2nd case is that we have a grid that has a first line `l` and a list of `lines`.
Since we don't want to `skew` the first line, we return it.
We use the 'cons' operator to concatenate a list of lines that have been indented `indent` and we declare that `indent` is a function that takes a `line` and returns a line with the `'_'` char prepended to it.

Because we want to add a `'_'` for every line, we recurse the tail of the list `ls`

```
skew (l:ls) = l : skew (map indent ls)
    where indent = '_' : line
```

Finally if we run the `transpose` function over ` skew (map indent ls)`, we'll get something that we can work with.

This is not the complete solution. This only searches diagonally in one direction, we need both.
`diagonal2 = transpose (skew (map reverse horizontal ))`

Now we refactor:

`diagonalize` takes a **type** `Grid` and returns a type `Grid`. Basically this function extracts the `transpose (skew grid)` common to both functions and use it to simplify our `getLines` function.

This pattern of calling a function `skew` on a parameter and then calling another function (i.e. `transpose`) is very common in Haskell and is refered to as "composing" two functions together, which can be done using the `.` ("dot")
operator.

`diagonalize grid = (transpose . skew) grid`

Now that we've re-written `diagonalize`, we can simplify our function by eliminating `grid` from both sides of the equation and we can see that
`diagonalize` is the _composed_ (i.e. `.`) function of `transpose` and `skew`.

## Making the game interactive

### Adding coordinates to the list, working with infinite lists

We need coordinates (x,y) of each character on the grid.

#### Infinite lists

**Ranges**

```
[0 .. 7]
// [0,1,2,3,4,5,6,7]
[0, 2.. 100]
[0, 3.. 100]
[10, 15..100]
```

**Infinite list**

```
[1 .. ]
// [1,2,3 .. ]
```

```
head [1..]
-- 1
head $ tail [1..]
-- 2
head . tail . tail [1..]
-- 3
[1..] !! 0
-- 1
```

```
take 10 [1..]
// [1,2,3,4,5,6,7,8,9,10]
```

```
take 10 $ map (*2) [1..]
[2,4,6,8,10,12,14,16,18,20]
```

```
takeWhile (<10) $ map (*2) [1..]
[2,4,6,8]
```

### The `list` Monad notation

```
mapped = do
    i <- [0..9]
    return (i * 2)
// [0,2,4,6,8,10,12,14,16,18]
```

The list monad can be used in an infinite way.
We can also use filter in the same way. We can use a `guard` close to add a boolean expression that tests type.
`:t guard`
`guard :: GHC.Base.Alternative f => Bool -> f ()`
`guard True :: [()]`
`[()]`
`guard False :: [()]`
`[]`
`guard True :: Maybe ()`
`Just ()`

```
filtered = do
    i <- [0..]
    return (i * 2)
```

Finally, we can combine map and filtered with infinite lists

```
div2 x = x `mod` 2 == 0
mappedAndFiltered = do
    i <- [0..]
    guard (div2 i)
    return (i + 1)
```

#### List comprehensions

This syntax is not necessarily compact, that's why Haskell provides list comprehensions. These are written in mathematical notation:
`[ i * 2 | i <- [0..9]]`
`[0, 2, 4, 6, 8, 10, 12, 14, 16, 18]`

And we can add our `guard` with a `,` comma **after** the list we are iterating over:
`[i \* 2 | i <- [0..9], div2 i]`

#### Repeating a sequence of rows or columns

In our grid:
Example
[(0,0), (0,1)]
[(1,0), (1,1)]
[(2,0), (2,1)]

The row number stays the same along the row and the column number stays the same along the column.

The `repeat` function will return an infinite list of a single value.
`take 10 $ repeat 1`
`[1,1,1,1,1,1,1,1,1,1]`

There's also a `cycle` function which allows us to pass a list and the value of that list will be repeated over and over.
`take 10 $ cycle [1,2,3]`
`[1,2,3,1,2,3,1,2,3,1]`

That does mean that cycling over a list of a single element is esentially the same as the repeat function.

If we map `repeat` over a list, we get a list of lists.
`map (take 10 . repeat)[0..10]`
`[[0,0,0,0,0,0,0,0,0,0],[1,1,1,1,1,1,1,1,1,1],[2,2,2,2,2,2,2,2,2,2],[3,3,3,3,3,3,3,3,3,3],[4,4,4,4,4,4,4,4,4,4],[5,5,5,5,5,5,5,5,5,5],[6,6,6,6,6,6,6,6,6,6],[7,7,7,7,7,7,7,7,7,7],[8,8,8,8,8,8,8,8,8,8],[9,9,9,9,9,9,9,9,9,9],[10,10,10,10,10,10,10,10,10,10]]`

This is very similar to the list of **row** numbers we need for our Grid.

#### Using zip to combine rows and columns

The `zip` function takes a list of type `a` and another list of type `b` and will return a list of tuples of type `[(a,b)]`.
`:t zip`
`zip :: [a] -> [b] -> [(a,b)]`
`zip[1..10]['a'..'z']`
`[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j')]`

**In order to create a list of coordinates:**

- We can use a loop construct
  `coords2 = do row <- [0..7] col <- [0..7] return (row,col)`
  `[(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]`

This list does not work as it is only one list and we need a list of lists.

```
    coords3 = do
        row <- [0..7]
        do
            col <- [0..7]
            return (row, col)
```

```
[[(0,0)],[(0,1)],[(0,2)],[(0,3)],[(0,4)],[(0,5)],[(0,6)],[(0,7)],[(1,0)],[(1,1)],[(1,2)],[(1,3)],[(1,4)],[(1,5)],[(1,6)],[(1,7)],[(2,0)],[(2,1)],[(2,2)],[(2,3)],[(2,4)],[(2,5)],[(2,6)],[(2,7)],[(3,0)],[(3,1)],[(3,2)],[(3,3)],[(3,4)],[(3,5)],[(3,6)],[(3,7)],[(4,0)],[(4,1)],[(4,2)],[(4,3)],[(4,4)],[(4,5)],[(4,6)],[(4,7)],[(5,0)],[(5,1)],[(5,2)],[(5,3)],[(5,4)],[(5,5)],[(5,6)],[(5,7)],[(6,0)],[(6,1)],[(6,2)],[(6,3)],[(6,4)],[(6,5)],[(6,6)],[(6,7)],[(7,0)],[(7,1)],[(7,2)],[(7,3)],[(7,4)],[(7,5)],[(7,6)],[(7,7)]]
```

This also doesn't work.

`return` wraps a value in the **List Monad**. What we need to do is `return` the entire content of the inner `do` expression **within** the List Monad by wrapping it within a list:

```
coords4 = do
    row <- [0..7]
    return $ do
        col <- [0..7]
        return (row, col)
```

The important thing to notice is that the `do` block isn't special apart from enabling the `<-` syntax. It's simply a **first class expression** that can be passed to other functions like `return`.

`[[(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7)],[(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7)],[(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7)],[(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)],[(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7)],[(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7)],[(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7)],[(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]]`

Now we have an _outer_ list which contains repeated iterations of the _inner_ list on **every row**.

- The **other** way we can create a list of coordinates, is by using **list comprehension** syntax:

We know:

- we want to return a _tuple_ of row and column
- we iterate the column over the numbers 1..7
- and the row over the numbers 1..7
  `[ (row, col) | col <- [0..7], row <- [0..7]]`

This however, suffers the same problem as our original `do` block. We need to wrap the inner statement in a **nested** List comprehension.

`[[(row,col) | col <- [0..7]] | row <- [0..7]]`

This will give us the list of coordinates we need.

**We can now use `zip` to generate our Grid.**

Our columns are equal to the list 0 to infinity repeated over and over again
`cols = repeat [0..]`
Rows are esentially _mapping_ over the list [0..] with the `repeat` function:
`rows = map repeat [0..]`

Using an infinite list of columns and rows allows us to create a Grid of arbitrary width and height.

```
cols = repeat [0..]
rows = map repeat [0..]
repeat 8 = take 8 . repeat
cols8 = repeat8 [0..]
rows8 = map repeat8 [0..7]
```

We can combine both lists with the `zip` function.
We'll get a list of tuples. These tuples contain the coordinates for a row and a column.

We can't just `zip rows8 cols8` because that would just zip the whole rows together
`zip rows8 cols8`

```
([0,0,0,0,0,0,0], [0,1,2,3,4,5,6,7])
([1,1,1,1,1,1,1], [0,1,2,3,4,5,6,7])
...
```

We need to to the same, but instead of combining the two lists with a comma, it zipped them together.
The comma `(,)` operator takes 2 arguments and combines them both with a comma in a new two value tuple.
`zipWith` is a more general version of `zip`. If we call `zipWith` with the comma `(,)` operator and our two lists, it does exactly the same as `zip`.
So what we want to do now is call `zipWith` with a function that combines the row from 'rows' and the row from 'cols', which is `zip`.
`zipWith zip rows8 cols8`
`[[(0,0),(0,1),(0,2)...], [(1,0),(1,1),(1,2)...]]`

We can extract this into a function called `zipOverGrid`
`zipOverGrid = zipWith zip`.

Now what we want is to associate every character in our **Grid** with its coordinates.
`zipOverGrid` takes a Grid of type `a` and a Grid of type `b` and returns a new Grid with the **tuple type** `(a,b)`.
`:t zipOverGrid`
`zipOverGrid :: [[a]] -> [[b]] -> [[(a,b)]]`

If we now use `zipOverGrid coords2 grid` we get exactly what we are looking for, a Grid where each character is associated with a coordinate in the form
`[((0,0),'_'), ((0,1), '_'), ((0,2), 'C') ...]`
`[((1,0),'_'), ((1,1), '_'), ((1,2), 'S') ...]`

We can now use our infinitely large set of rows and cols that we declared earlier `cols = [0..], rows = [0..]` and `zipOverGrid` to define a Grid with infinite coordinates
`coordsInf = zipOverGrid rows cols`

And now we can call `zipOverGrid` on our infinite coordinates grid `coordsInf` and our grid of words `grid` to get our Jumble.

We're going to add **parameterized type synonyms** to create different types of Grid.
We're going to create a new data type called Cell.
We're going to incoorporate our coordinate grid.

#### Cell data type

This will have a constructor `Cell` and it will take a tuple of `Integer`s and a `Char` and we'll add deriving `Eq`, `Ord` and `Show` so that we can compare Cells and display them on the screen.
`data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)`

We know that if we run our `zipOverGrid` function with the coordinates grid, we return a tuple of coordinates and chars for each position in the grid.

`zipOverGrid coords grid`
`[[((0,0), '_')...]]`

However, now that we have a `Cell` data type, this is not the result we want. What we want is a Cell data type with (0,0) and '_'
`Cell (0,0) '_'`

We saw that `zipOverGrid` uses `zipWith` and `zip`.
Previously, when we created our grid of rows and columns, we used `zipWith` instead of `zip` in order to not simply join values with a comma, but allow us to pass an arbitrary function.

Our data type Cell, is a constructor function which takes in an Integer and a Char and returns a Cell
`Cell :: (Integer, Integer) -> Char -> Cell`
So insted of joining our values of the tuple (e.g. (0,0)) and the Char (e.g. ('C')) with a comma `(,)`,

`(zipWith zip) coords grid == (zipWith(zipWith(,)) coords grid`

we can now join them together with Cell instead

`(zipWith(zipWith Cell)) coords grid`

With this in mind we can write a new function `zipOverGridWith` and it would take a function `f`, a grid `a` and another grid `b` and it would run `(zipWith(zipWith f)) a b`

`zipOverGridWith f a b = (zipWith(zipWith f)) a b`

**REFACTORING**

`zipOverGridWith f = (zipWith(zipWith f))`
`zipOverGridWith f = zipWith(zipWith f)`

Now we see a pattern identical to a **list composition** so we can separate `zipWith` with a dot `(.)` operator and separate the argument with the dollar operator `$`.

`zipOverGridWith f = zipWith . zipWith $ f`

Now we have `f` on the left and right of both sides

`zipOverGridWith = zipWith . zipWith`

`zipOverGridWith Cell coords grid`
`[[... Cell (0,1), '_', Cell (0,2), 'C', ... ]]`

We have our Grid.

We declare our `coordsGrid` using a `let` clause.

```coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols
```

We can define a function that takes in a Grid and returns a Grid full of cells.
`gridWithCoords grid = zipOverGridWith Cell coordsGrid grid`

Our definition of the data type `Grid` is a list of strings.
`type Grid = [String]`
Which we can think of as a grid of Chars
`type Grid = [[Char]]`

What we want now is to turn our `Grid` type into a **parameterized type**, that is to say, a Grid of _any_ type of character

`type Grid a = [[a]]`

Now we can start treating each Grid as being a grid of a different type.

This means we need to **refactor** our data types.

We can use `ghci` to help us by showing us the error messages.
These errors are because we've declared certain things as `Grid` expecting it to be a grid of `Chars` were now it is a parameterized type that needs to be specialized, so we need to tell it, every time we are using a `Grid` of `Char`s.

We can start by going over every instance of `Grid` in our code and adding `Char`.

Now we can use our parameterized `Grid` to give types to the new functions that we've created.

For example

`zipOverGrid` takes a `Grid` of type `a` and a `Grid` of type `b` and returns a list of tuples `Grid (a, b)`

`zipOverGrid :: Grid a -> Grid b -> Grid (a,b)`

`zipOverGridWith`'s first function takes value of type `a` and a value of type `b` and returns a value of type `c` and that's reflected with the input grids `Grid a` and `Grid b` and the final output `Grid c`.

`zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c`

`coordsGrid` is a value, not a function, but it's still a good idea to give it a type.

`coordsGrid :: Grid (Integer,Integer)`

Our `gridWithCoords` function takes in a `Grid` of `Chars` and returns a `Grid` of `Cell`s
`gridWithCoords grid = zipOverGridWith Cell coordsGrid grid`

Our problem now is that right now `outputGrid` is expecting a `Grid` of `Char` and not a grid of `Cell`

`outputGrid :: Grid Char -> IO ()`

In `/app/Main.hs`

```module Main where

import Data (grid, languages)
import Lib

main :: IO ()
main = outputGrid grid
```

In `main = outputGrid grid`, `grid` is a `Grid` of `Char`.
Let's create a grid with coordinates `gwc`

```
let gwc = gridWithCoords grid
in outputGrid gridWithChoords
```

And modify `outputGrid`

```
outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)
```

`outputGrid` calls `formatGrid`, so we'll change that too.
'formatGrid' is currently defined as the `unlines` function, which joins a list of strings.
But we don't have a list of strings now but a list of lists of cells. So we need to declare helper functions.

`cell2char` outputs a `Char` when given a `Cell`
`cell2char :: Cell -> Char`

We can use **pattern matching** to match on a Cell and its coordinates tuple. We don't care about the coordinates tuple so we used the underscore `_` for the coordinates and the character `c` and we'll return `c`.
`cell2char (Cell _ c) = c`

We also need to create a `Char` grid from our `Cell` grid and return `unlines` of that grid of chars.

```
formatGrid :: Grid Cell -> String
   formatGrid =
    let charGrid = undefined
    in unlines charGrid
```

To turn a grid of Cells to a grid of Chars we can look at the `zipOverGrid` function where we call `zipWith zip` and `zipOverGridWith` is `zipWith` composed with `zipWith`

`zipOverGrid :: zipWith zip`
`zipOverGridWith = zipWith . zipWith`

In a similar way, if we had a single line, we would call `map cell2char grid`

`charGrid = map cell2char grid`

But we have a **grid** so we can try substituting `map` for `(map . map)`

`charGrid = (map . map) cell2char grid`

and it works as expected.

```
formatGrid :: Grid Cell -> String
formatGrid grid =
  let charGrid = (map . map) cell2char grid
   in unlines charGrid
```

We'll define `mapOverGrid` as a helper function to replace `(map . map)`.
This gives us the opportunity to add a type signature so we can see explicitly what the type of this expression is.

`mapOverGrid :: (a -> b) -> Grid a -> Grid b`
`mapOverGrid = map . map`

Now that we have a way to take a Grid of Cells into a Grid of Chars, we can simplify `formatGrid`

From this:

```
formatGrid :: Grid Cell -> String
formatGrid grid =
  let charGrid = mapOverGrid cell2char grid
   in unlines charGrid
```

To this

```
formatGrid :: Grid Cell -> String
formatGrid grid = unlines (mapOverGrid cell2char grid)
```

And we can see again a function composition pattern, so we can simplify further

`formatGrid grid = unlines . mapOverGrid cell2char $ grid`
`formatGrid = unlines . mapOverGrid cell2char`

**MORE REFACTORING**

```
getLines :: Grid Char -> [String]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)
```

can easily be refactored into a grid of Cell:

```
getLines :: Grid Cell -> [[Cell]]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)
```

We need to make sure that all other functions included in the `getLines` function are also refactored to take in a `Cell` type insted of `Char`,
e.g.

From

`diagonalize :: Grid Char -> Grid Char`

To

`diagonalize :: Grid Cell -> Grid Cell`
`diagonalize = transpose . skew`

Now the `skew` function needs to be refactored. The compiler points us towards the colon `(:)` operator. We use the `(:)` to concatenate a '\_' character with a line:

```
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line
```

This won't work because `line` is now a list of `Cell`

`lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2`

However, we can generalize our skew function because all we are doing is passing in an indentation:

```
skew (l:ls) = l : skew (map indent ls)
    where indent line = Indent : line
```

What we have to do now is to look for our definition of `Cell`:

`data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)`

We need to add an alternative, which is the constructor `Indent` which doesn't need to take in any parameters:

```
data Cell = Cell (Integer, Integer) Char
           | Indent
             deriving (Eq, Ord, Show)
```

Even though `getLines` itself didn't have a `Char` constraint, it's being called from another function `findWord` that is expecting `Char`

`findWord :: Grid Char -> String -> Maybe String`

Now we **refactor** the `findWord` function.
Now the `Grid` takes a `Cell`, `String` is the word we are searching for, so it stays `String` and the result now needs to be a `Maybe` list of cells which will be the word and the coordinate positions.

`findWord :: Grid Cell -> String -> Maybe [Cell]`

Next is `findWordInLine` which takes in a `String` which is the word being searched for, another `String` which is the line and returns a Boolean.

`findWordInLine :: String -> String -> Bool`

We can start to refactor by taking the second `String` and turning it into a list of `Cell`.

`findWordInLine :: String -> [Cell] -> Bool`

However, now `isInfixOf` in `findWordInLine = isInfixOf` will not work because
`:t isInfixOf` takes a list of `a` and a list of `a` and returns a `Bool`.
`isInfixOf :: Eq a => [a] -> [a] -> Bool`

For now, we'll leave it `undefined`.

Let's move on to the `findWords` expression.

`findWords :: Grid Char -> [String] -> [String]`

We can start by changing the Grid of Char to a Grid of Cell.
We still want a list of strings, as they are the words to be found.
And we want it to return a list of lists of cells.

`findWords :: Grid Cell -> [String] -> [[Cell]]`

Previously, we simply returned a `Bool` when we `foundWordInLine` and then it returned the word itself.
Now we need to return a `Maybe` list of `Cell`. Which means that `findWordInLine` is going to need to return a `Maybe` list of `Cell`

`findWordInline :: String -> [[Cell]] -> Maybe [[Cell]]`

However, now mapping `findWordInLine` over `lines` wouldn't return a list of boolean values but a list of `Maybe` values.

### Searching the grid with a custom recursive function

#### Replacing `isInfixOf` with a new data structure

To do this we will create two kinds of **recursive** function:

- A recursive search by prefix
- Build this recursive search into a recursive infix search

The problem now is that where before in our grid we had

```
["__C________R___","__SI________U__","__HASKELL____B_","__A__A_____S__Y","__R___B___C____","__PHP____H_____","____S_LREP_____","____I__M_Y__L__","____L_E__T_O___","_________HB____","_________O_____","________CN_____"]
```

whereas now we have

```
[
    [Cell (0,0) '_',Cell (0,1) '_',Cell (0,2) 'C',Cell (0,3) '_',Cell (0,4) '_',Cell (0,5) '_',Cell (0,6) '_',Cell (0,7) '_',Cell (0,8) '_',Cell (0,9) '_',Cell (0,10) '_',Cell (0,11) 'R',Cell (0,12) '_',Cell (0,13) '_',Cell (0,14) '_'],[Cell (1,0) '_',Cell (1,1) '_',Cell (1,2) 'S',Cell (1,3) 'I',Cell (1,4) '_',Cell (1,5) '_',Cell (1,6) '_',Cell (1,7) '_',Cell (1,8) '_',Cell (1,9) '_',Cell (1,10) '_',Cell (1,11) '_',Cell (1,12) 'U',Cell (1,13) '_',Cell (1,14) '_'],
    [Cell (2,0) '_',Cell (2,1) '_',Cell (2,2) 'H',Cell (2,3) 'A',Cell (2,4) 'S',Cell (2,5) 'K',Cell (2,6) 'E',Cell (2,7) 'L',Cell (2,8) 'L',Cell (2,9) '_',Cell (2,10) '_',Cell (2,11) '_',Cell (2,12) '_',Cell (2,13) 'B',Cell (2,14) '_'],
    [Cell (3,0) '_',Cell (3,1) '_',Cell (3,2) 'A',Cell (3,3) '_',Cell (3,4) '_',Cell (3,5) 'A',Cell (3,6) '_',Cell (3,7) '_',Cell (3,8) '_',Cell (3,9) '_',Cell (3,10) '_',Cell (3,11) 'S',Cell (3,12) '_',Cell (3,13) '_',Cell (3,14) 'Y'],
    [Cell (4,0) '_',Cell (4,1) '_',Cell (4,2) 'R',Cell (4,3) '_',Cell (4,4) '_',Cell (4,5) '_',Cell (4,6) 'B',Cell (4,7) '_',Cell (4,8) '_',Cell (4,9) '_',Cell (4,10) 'C',Cell (4,11) '_',Cell (4,12) '_',Cell (4,13) '_',Cell (4,14) '_'],
    [Cell (5,0) '_',Cell (5,1) '_',Cell (5,2) 'P',Cell (5,3) 'H',Cell (5,4) 'P',Cell (5,5) '_',Cell (5,6) '_',Cell (5,7) '_',Cell (5,8) '_',Cell (5,9) 'H',Cell (5,10) '_',Cell (5,11) '_',Cell (5,12) '_',Cell (5,13) '_',Cell (5,14) '_'],
    [Cell (6,0) '_',Cell (6,1) '_',Cell (6,2) '_',Cell (6,3) '_',Cell (6,4) 'S',Cell (6,5) '_',Cell (6,6) 'L',Cell (6,7) 'R',Cell (6,8) 'E',Cell (6,9) 'P',Cell (6,10) '_',Cell (6,11) '_',Cell (6,12) '_',Cell (6,13) '_',Cell (6,14) '_'],
    [Cell (7,0) '_',Cell (7,1) '_',Cell (7,2) '_',Cell (7,3) '_',Cell (7,4) 'I',Cell (7,5) '_',Cell (7,6) '_',Cell (7,7) 'M',Cell (7,8) '_',Cell (7,9) 'Y',Cell (7,10) '_',Cell (7,11) '_',Cell (7,12) 'L',Cell (7,13) '_',Cell (7,14) '_'],
    [Cell (8,0) '_',Cell (8,1) '_',Cell (8,2) '_',Cell (8,3) '_',Cell (8,4) 'L',Cell (8,5) '_',Cell (8,6) 'E',Cell (8,7) '_',Cell (8,8) '_',Cell (8,9) 'T',Cell (8,10) '_',Cell (8,11) 'O',Cell (8,12) '_',Cell (8,13) '_',Cell (8,14) '_'],[Cell (9,0) '_',Cell (9,1) '_',Cell (9,2) '_',Cell (9,3) '_',Cell (9,4) '_',Cell (9,5) '_',Cell (9,6) '_',Cell (9,7) '_',Cell (9,8) '_',Cell (9,9) 'H',Cell (9,10) 'B',Cell (9,11) '_',Cell (9,12) '_',Cell (9,13) '_',Cell (9,14) '_'],
    [Cell (10,0) '_',Cell (10,1) '_',Cell (10,2) '_',Cell (10,3) '_',Cell (10,4) '_',Cell (10,5) '_',Cell (10,6) '_',Cell (10,7) '_',Cell (10,8) '_',Cell (10,9) 'O',Cell (10,10) '_',Cell (10,11) '_',Cell (10,12) '_',Cell (10,13) '_',Cell (10,14) '_'],
    [Cell (11,0) '_',Cell (11,1) '_',Cell (11,2) '_',Cell (11,3) '_',Cell (11,4) '_',Cell (11,5) '_',Cell (11,6) '_',Cell (11,7) '_',Cell (11,8) 'C',Cell (11,9) 'N',Cell (11,10) '_',Cell (11,11) '_',Cell (11,12) '_',Cell (11,13) '_',Cell (11,14) '_']]
```

If we look at the word "HASKELL", where before we would've found 'H', now we find 'H',Cell (2,3).

Now if we look at the type of `isInfixOf` we can see that it takes two lists and returns a Bool.

`:t isInfixOf`
`isInfixOf :: Eq a => [a] -> [a] -> Bool`
So if we do

```
"HASKELL" `isInfixOf` "___HASKELL__"
```

it will return true. This is undesirable.

Instead we can use a related function `isPrefixOf` that fails for "**HASKELL**" but is true where HASKELL is at the beginning of the string.

That's a simpler function that we could implement first.

So we would like to check if the word "HASKELL" matched that portion of the line.

Let's start by defining a new function called `findWordInCellLinePrefix` which takes a String, _which is the word we are searching for_, and a list of `Cell`.

`findWordInLine` returned a `Bool`, we now need to return a `Maybe` list of `Cell` if it's found.

`findWordInCellLinePrefix :: String -> [Cell] -> Maybe [Cell]`

And we call it with the word and the line, we can think of the word as being a character `c` followed by a list of characters `cs`. However, because we then need to pass a line in the same way (A lines is a list of cells), we'll disambiguate by referring to the characters as `x:xs` and cells as `c:cs` and we'll add a guard clause where we compare `x` against `c`.

`findWordInCellLinePrefix (x:xs) (c:cs) | x == c`

However, `x` is a `Char` and `c` is a `Cell`. So we need to pass `c` through a function that turns cells to chars.

This branch of the function will only run if they compare
`findWordInCellLinePrefix (x:xs) (c:cs) | x == cell2char c`

and if they do, we'll return the cell followed by a recursive call into `findWordInCellLinePrefix` and we pass in the `xs` and the `cs`

```
findWordInCellLinePrefix :: String ->. [Cell] -> Maybe [Cell]
findWordInCellLinePrefix (x:xs) (c:cs) | x == cell2char c
    = c : findWordInCellLinePrefix xs cs
```

Then we need to write the base case
`findWordInCellLinePrefix _ _ = Nothing`

Because we are using the `Maybe` type, we return `Nothing`.

However, this won't work as `c` and `findWordInCellLinePrefix xs cs` are different types.

There's a common pattern used when working with recursive functions of this complexity, and that is to add an **accumulator**, a new list. This list will **accumulate** the values we are trying to pick up.

```
findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c
```

and now we can simply call the function again, pass in the **accumulator** and the tail of the two lists that we are comparing. And because we need the accumulator to accumulate something , we need to add the cell to that list.

```
findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c
    = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix _ _ = Nothing
```

In ` = findWordInCellLinePrefix (c : acc)` we use `c : acc` instead of `acc ++ [c]` because the `++` operator is a little inefficient compared to cons. The only problem with this solution is that now our list will be returned in the wrong order but it's very simple and efficient to reverse it on the other end.

Our second base case needs to split up into several base cases.

If we know that the lists of strings to compare is now an empty list, that means that we must've got to the end of the comparison, and that the comparison has matched, which means we can return the accumulator and because this is a successful call, we would return a `Just acc`.

`findWordInCellLinePrefix acc [] _ = Just acc`

If we have any other case, we can throw away the result of the `acc` (we match it with an `_`), we don't care what the rest of the string is because nothing matched so there's nothing left to do and we don't care what the rest of the line is and we can simply return `Nothing`.

`findWordInCellLinePrefix _ _ _ = Nothing`

In total we have **three** cases:
The case in which we **recurse** into the function, adding to the **accumulator**, this is a potentially good case.

```
findWordInCellLinePrefix acc (x:xs) (c:cs) = x == cell2char c
    = findWordInCellLinePrefix (c : acc) xs cs
```

The case where we know we got to the end of the match and we can return a good value.

`findWordInCellLinePrefix acc [] _ = Just acc`

And all the other cases where we've either gotten to the end and failed, we've run out of cells to match, etc., where we return `Nothing`.

Now we can go back to edit our `findWordInLine` function and write it in terms of `findWordInCellLinePrefix`

```
findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine = undefined
```

We start taking a `word` and a `line` and we can ask wether the `word` is found at the head of the line as a prefix (we pass through the accumulator `[]`, the `word` and the `line`).

`let found = findWordInCellLinePrefix [] word line`

And this new expression will be a `Maybe` value so we can **pattern match** using `case`

`in case found of`

if `Nothing` was found it's not necessarily a failure because we can recurse back into the `findWordInLine` function and now pass in `word` because the word we are searching for hasn't changed, but now we want to recurse into the `tail` of `line` so that we can check wether the `word` is found at the next position in the `line`.

`Nothing -> findWordInLine word (tail line)`

And if the cell was found, then we need to return that value

`Just cs -> Just cs`

Instead of writing `Just cs` twice, we can use the `@` pattern because we don't care about the value inside and we simply return that expression.

`cs@(Just _)-> cs`

```
findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine word line =
    let found = findWordInCellLinePrefix [] word line
    in case found of
        Nothing -> findWordInLine word (tail line)
        cs@(Just _) -> cs
```

Now we can get back to the refactor of the `findWord` function:

```
findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let lines = getLines grid
    found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing
```

We still need `getLines grid`. `foundWords` now returns a list of list of `Cell`.
`foundWords` will return a list of lists of `Cell`. The expression `foundWords` will in fact be a list of `Maybe` list of `Cell`, so if we call `catMaybes` on that list we will get a list of lists of `Cell`.

### Making the game playable with the IO Monad

### Modelling the Game with Haskell's data types

### Random letters

```

```
