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

### Searching the grid with a custom recursive function

### Modelling the Game with Haskell's data types

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

### Making the game playable with the IO Monad

### Random letters
