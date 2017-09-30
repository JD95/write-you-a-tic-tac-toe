# write-you-a-tic-tac-toe

## THIS IS A WORK IN PROGRESS AND NOT READY FOR USE AS LEARNING MATERIAL

This project is meant for programmers experience in some other language to put together the basic concepts of Haskell.

This project assumes knowledge of:
- git (how to clone a repo)
- A text editor with Haskell supported ([Atom](https://atom.io/), [Emacs](https://www.gnu.org/software/emacs/), [Vim](https://vim.sourceforge.io/), [Visual Studio Code](https://code.visualstudio.com/))

By the end of the project you'll have experience with:
- Basic arithmetic
- Basics of using stack (To build and test your code)
- Case statements and Pattern Matching
- Control flow with booleans
- Currying
- Define functions
- Defining your own types
- Function composition
- Lambdas
- Lists
- Printing to the console
- Reading user input from the console
- Representing Failure with Maybe
- Type Signatures

# Primitive Data Types 

The initial problem of programming is figuring out how to represent your problem in a way that the computer can understand it. A smart tactic is to first encode the objects involved with the problem and then attempt to encode  the transformations of those objects into the result.

### Int

- Numbers transfer directly into programming
- Almost all programming langues have integers as a datatype

### Double

- Represents decimal numbers

### Char

- Represents letters
- Based on numbers (ascii)
  - Can assign numbers to letters (a=1,b=2,c=3,...)
  - Need to represent all characters on keyboard so different scheme

### String

- Just a collection of characters

## Interactions

- Can represent coordinates with numbers
- String and Double can be units of measurement

## Synthesis
- How can you represent a person in a program?
- Height (Double)
- Age (Int)
- Name (String)
- Hair Color (String / Int)
- Gender (Char)
- Types can be used to build up a representation of something in the real world
- We can thus "encode" real things into our programs

# Lambdas (Meta)

Lambdas represent transformations from values of one type to values of another.

### Syntax

- \ represents the lambda symbol
- The symbols after the \ are the names of the inputs to the function
- The (->) represents transforming the values on the left into the values on the right
- `(\x -> x)` this is the simplest transformation because it does nothing
  - This is called the identity function
- `(\alpha -> "beta")` here we take the value alpha and turn it into the String "beta"
- Common math operations are also allowed
- `(\x -> (x + 1) * (3 / x) - 4)`
  - Parenthesis group things just like in math
- The function (++) lets you combine two Strings `(\first -> (\last -> first ++ last))`
- The function (:) lets you attach a Char to the front of a string `(\x -> x : "ookies!")`

### Type Signature

- Lambda functions are transformations of values from one type to, potentially, another type
- The syntax for the type signature for a function called addOne which takes an Int and transforms it into an int would be `(name :: Int -> Int)`
  - Notice how the (->) symbol matches up with the arrow in the lambda

### Multiple Arguments

- In order to pass multiple values into a function, you'll have to nest lambda functions
- For example `(\x -> (\y -> x + y))`, which takes a value x and then transforms it into a lambda function which takes a value y and transforms it into the expression `x + y`

## Interactions

- The math operators (+,-,/,*) are just lambdas with symbols for names
- You can create your own lambdas with funny names by putting the symbol in ()
- There is a shortcut for making a lambda with multiple arguemsnts `(\x y -> x + y)`

## Synthesis

- What kinds of transformations can we represent with lambdas?
- Area of a rectangle `(\height width -> height * width)`
- Area of a circle `(\radius -> 3.14159265 * (radius * radius))`

# Function Evaluation 

### Function Application
- Functions are no good if we can't simplify them
- Function application lets us turn (1 + 1) into two
  - `("Hello " ++ "World")` into `"Hello World"`
- More generally function application looks like this

```haskell
(\x -> x + 1) 1
```

 When a value is to the right of a function, it is the value which will be subsituted in for the lambda

```haskell
(\1 -> 1 + 1)
1 + 1
2
```

Functions with multiple arguements simply apply them one at a time

```haskell
(\x (\y -> x + y)) 5 5
(\5 (\y -> 5 + y)) 5
(\y -> 5 + y) 5
(\5 -> 5 + 5)
5 + 5
10
```

- Multi-argument lambdas work in exactly the same way (since they are the same)
  - (\first last -> first ++ " " ++ last) "Tom" "Riddle"
  - (\"Tom" last -> "Tom" ++ " " ++ last) "Riddle"
  - (\"Tom" "Riddle" -> "Tom" ++ " " ++ "Riddle")
  - "Tom" ++ " " ++ "Riddle"
  - "Tom Riddle"

### Bool
- There is another data type which is useful for evaluation
- Boolean values or Bool in Haskell represent either True or False
- These truth values let us encode logic into our programs

### Case Expressions

Case expressions allow you to check what the value used as the input to a lambda is and choose between several transformations

```haskell
isZero = (\x -> case x of { 0 -> True; _ -> False; })
```

This lambda takes some Int x and checks whether or not it is 0 and gives a True if it is and for anything else (the _ symbol matches anything)it returns False

This lambda would be useful in a situation like this

```haskell
divide = (\x y -> case isZero y of { True -> 0; False -> x / y; })
```

While this isn't the best way to solve the problem of dividing by zero, we can put a bandaid on it like this


### If Then Else
- Like multi-arguement functions there is a shorthand way to write case expressions that choose results based on a Bool value
- The syntax would be like this `(\x y -> if isZero y then 0 else x /y; })`
  - Which reads a bit better than the case expression

### Basic Comparisons
- There are some built in functions which deal with generating True or False values. These "logic" and "comparison" operators are as follows
- `==` checks if the values are the same
- `>` and `<` have the same function as in math
- `>=` and `<=` mean greater than or equal to and lesser than or equal too
- `not` is a function that flips the boolean, True becomes False and visa versa
- `&&` means logical and, it takes two Booleans and, if they are both True, also returns true (anything else is False)
- `||` means logical or, it takes two Bool values and if either are True it will return True, False otherwise

## Interaction

What happens if you don't give a function all of it's inputs? Remember that evaluation just supsitutes the values one at a time. There is no reason stopping half way should break anything.

```haskell
(\x y -> x + y) 5
(\5 y -> 5 + y)
```

- Given that functions like `+` and `||` are still *just* functions, we can do the same for them

`(5 +) = ((+) 5) = (\x -> 5 + x)`

- If you have a bunch of functions applied to eachother (a (b (c (d input)))), you can rewrite it without the parenthesis using the ($) function. So `a $ b $ c $ d $ input`

## Synthesis
- Lambdas are data types just like String, Int, and Bool, is there a way to add them?
- `(\g f -> (\x -> g (f x)))`
- There is already a function that does this called (.)
- `g . f = \x -> g (f x)`
- From before with many functions applied to the results of other functions we can rewrite to something like this: `a . b . c . d $ input`
  - Here, everything to the left of $ is the composition of a, b, c, and d
  - The input value will flow left through each of these functions, getting transformed by each

```haskell
(\x -> x + 1) . (\x -> x + 2) . (\x -> x + 3) $ 5
(\x -> x + 1) . (\x -> (x + 3) + 2) $ 5
(\x -> ((x + 3) + 2) + 1) $ 5
(\5 -> ((5 + 3) + 2) + 1)
((5 + 3) + 2) + 1
(8 + 2) + 1
10 + 1
11
```

# Generating Lists (Ana)

`[1..]`

This is the notation for defining a list of all numbers from 1 to infinity.

`take :: Int -> [a] -> [a]`

Gives a list with only the first n elements of the list.

`takeWhile :: (a -> Bool) -> [a] -> [a]`

Uses elements from the front of the list while the predicate is satisfied.

`drop :: Int -> [a] -> [a]`

Gives a list without the first n elements.

`dropWhile :: (a -> Bool) -> [a] -> [a]`

Uses elements from the front of the list while the predicate is satisfied.

`iterate :: (a -> a) -> a -> [a]`

Uses a some function and iterates it on the initial value a.

`repeat :: a -> [a]`

An infinite list with repeating values of a.

`replicate :: Int -> a -> [a]`

A finite list with repeating values of a.

`cycle :: [a] -> [a]`

Given some list, create an infinite list with the inital list repeating.

## Interaction
- Drop and iterate can be used to grab sections of a list
  - What are the first five letters after g?
  - take 5 . dropWhile ((/=) 'g') $ ['a'..]
- Iterate lets us generate arithmetic and geometric sequences

## Synthesis
- You can use individual elements and simple rules to generate finite and infinte lists of values. Often times, this will be your method of setting up other functions.

# Transforming Lists (Meta)

`map :: (a -> b) -> [a] -> [b]`

Transforms each element of a list using some function.

`filter :: (a -> Bool) -> [a] -> [a]`

A new list with only the elements that satisfy the predicate.

`reverse :: [a] -> [a]`

A list with the elements reversed.

`intersperse :: a -> [a] -> [a]`

A list with some value placed between each element.

`intercalate :: [a] -> [[a]] -> [a]`

A list with another list placed between each element.

`transpose :: [[a]] -> [[a]]`

Rotates a list of lists. A 3x2 list becomes a 2x3.

`subsequences :: [a] -> [[a]]`

All possible subsequences of a list

`permutations :: [a] -> [[a]]`

All possible permutations of the elements of a list.

## Interaction
- map and filter can be used "query" lists

## Synthesis
- List comprehensions are shorthands for this. Most programs will consist of generating a basic list of values, then transforming that list into something more suited for your particular problem.

# Folding Lists (Cata)
## Exposition
`foldr :: (b -> a -> b) -> b -> [a] -> b`

Using some function and an inital value, combine all the elements of a list.

`concat :: [[a]] -> [a]`

Given a list of lists, create a new list with all the elements from each list.

`concatMap :: (a -> [b]) -> [a] -> [b]`

Apply some function which generates a list to each element and then concatenate the results.

`and :: [Bool] -> Bool`

Given a list of Bool values, fold the list using &&.

`or :: [Bool] -> Bool`

Given a list of Bool values, fold the list using ||.

`any :: (a -> Bool) -> [a] -> Bool`

Whether any value of some list satisfies some predicate.

`all :: (a -> Bool) -> [a] -> Bool`

Whether all elements of a list satisfy a predicate.

`mapM_ :: (a -> IO b) -> [a] -> IO ()`

Perform an ~IO~ action using every element of a list.

`forM_ :: [a] -> (a -> IO b) -> IO ()`

The same as ~mapM_~ but with the arugements flipped.

## Interaction
- summations
- sequences
- validating a list of results

## Synthesis

Folds can collapse a list down to a single result, but they can also be thought of as "sequencing" events, as in forM_ and mapM_. So lists are not only a means of holding "values", but also a means of structuring the execution of IO actions.

