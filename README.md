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

# Intro

All real world problems have two things: nouns and verbs. We want *cars* to *drive themselves*. We want to *recognize* the *faces* in *pictures*. The process of programming involves converting the problems into a language that the computer can understand and act on. Unfortunately, computers aren't yet at the point to be able to completely understand human intent, so programmers still have jobs (for now). For example, how could we represent a person? Obviously we could model every *single* molecule in their body, but programmers are lazy and often the problems we solve don't care about *that* level of specificity. Lets think about what information you might use to describe a person you saw to someone else.

They were:
- Tall
- Blond Hair
- Kinda Heavy
- Had Green Eyes

That's a fairly good description, probably enough to identify someone with. Computers like specific details whenever possible though, so lets modify our description a bit.

They were:
- Height: 6 feet, 3 inches
- Hair Color: Blond
- Weight: 210 pounds
- Eye Color: Green
- Name: Taylor Doe
- Age: 27 years

So more specific in this case meant including numbers and units in a few cases. Computers like numbers, a lot. People on the other hand want to be more expressive. To this end, Haskell comes with a few different kinds of values to talk about called "data types".

# Primitive Data Types 

### Int

- Numbers transfer directly into programming
- Almost all programming langues have integers

### Double

- Represents decimal numbers

### Char

- Represents letters
- Based on numbers (ascii)
  - Can assign numbers to letters (a=1,b=2,c=3,...)
  - Need to represent all characters on keyboard so different scheme

### String

- Just a collection of characters
- Useful for things like names

## The Types of Tic Tac Toe?

What types could we use to represent the game of tic tac toe?

Well, players usually are marked as either `X` or `O`, so that can be a `Char`. We could possibly represent the board using some kind of string:

```haskell
"xox"
"oox"
"xxo"
```

It's a start, but we have a ways to go yet.

# Functions

As was mentioned before, all programs are made of two things: nouns and *verbs*. In programming these two concepts are called data and functions. Now that we have a familiarity with how we can represent nouns with data types, lets think about how we can represent the actions taken within our program as functions.

In functional programming, all actions are mappings of some input value to some output value. Give me an apple, I'll give you an orange. Give me some money, I'll give you a cake. Since everything is a mapping, haskell uses something called a "lambda" expression to describe them succinctly.

```haskell
\ x y z -> x y (x z) 
```

### Syntax

- \ represents the lambda symbol
- The symbols immediately after the \ are the names of the inputs to the function
- The (->) represents transforming the values on the left into the values on the right

So what exactly can we represent with lambdas?

```haskell
(\x -> x)
```

This is the simplest transformation because it does nothing, normally called "identity" or id for short. Now, just because we are given an input, doesn't mean we have to actually use it.

```haskell
(\alpha -> "beta")
```
Here we take the value alpha and turn it into the String "beta" reguardless of what alpha actually is. Beyond these rather bland transformations, we also have the normal math operations available to us.

```haskell
(\x -> (x + 1) * (3 / x) - 4)
```
- Parenthesis group things just like in math

It's important to note that these symbols like `+` and `*` are functions just like lambdas are. They represent mapping numbers to other numbers. There is nothing special about these symbols though, you can use symbols to make your own functions later on as well, but don't worry about that at the moment as we continue to cover uses of the basic types.

If you remember that values of the `String` type are just collections of `Char` values, then it makes sense that we should be able to put too groups together.

```haskell
(\first -> first ++ " is the best!")
```

Here, we could map something like "Pie" to "Pie is the best!". What if we had a `String` but only wanted to add a single `Char` to it? The function `(:)` lets you attach a `Char` to the front of a `String`.

```haskell
(\x -> x : "ookies!")
```

### Type Signature

Now, just because we haven't been writing out what the types of the inputs and outputs are explicitly, they have been there all along. Haskell is smart enough to figure out what you are talking about based on what you actually do with the inputs. For example, if you were to use `+` the compiler would know that your inputs had to be numbers because it doesn't make sense to add things which are not numbers.

Programmers are human, we like to not have to think as hard as the compiler when just reading the code. To this end, we can actually give names to our lambda values and also explicitly write out their types. Sometimes these even helps the compiler figure out what we mean if we are too ambiguous.

The syntax for the type signature for a function called addOne which takes an Int and transforms it into an int would be:

```haskell
addOne :: Int -> Int
addOne = \ x  -> x + 1
```
The top line reads "addOne has the type Int to Int". The `::` is read as "has the type". Notice how the (->) symbol matches up with the arrow in the lambda. There is no rule that says they must visually line up, but I've written them like this to draw the parallel with how the arrow is used in the *value* of the lambda and the *type*.

### Multiple Arguments

So far we have only seen lambdas which take a single input, which is not always what we want. A simple example of a function which needs two inputs is `+`. However, we are restricted by the fact that lambdas can *only* represent mappings from a single input to a single output. Luckily, we can exploit the fact that in Haskell, lambdas are values just like anything else.

In order to pass multiple values into a function, you'll have to nest lambda functions. For example:

```haskell
add = (\x -> (\y -> x + y))
```

`add` takes a value x and then transforms it into a lambda function which takes a value y and transforms it into the expression `x + y`. In other terms, we begin with a vague expression "some number plus some other number". Then once we know one of the numbers we can say something like "5 plus some number". This is more specific, but we still need more details to give a concrete result. The final input lets us say "5 plus 6" which can be reduce to it's answer "11".

Writing our functions like this is cumbersome though, and there is a shortcut for making a lambda with multiple arguments.

```haskell
(\x y -> x + y)
```


## Function Evaluation 

A question you might be asking is, "how does the computer actually *do* something with these lambda thingies"? The actual, low level answer to that question is complicated, but there is a way to explain it that gives you a mental model of what is happening.

Say we are given a lambda and some value we want to use as input:

```haskell
(\x -> x + 1) 1
```

 When a value is to the right of a function, it is the value which will be subsituted in for the lambda's input. Once the value is substituted, we can drop the lambda and then evaluate our new expression.  

```haskell
(\1 -> 1 + 1)
1 + 1
2
```

Math functions work exactly as they should and reduce to the resulting numbers. Functions with multiple arguements simply apply them one at a time.

```haskell
(\x (\y -> x + y)) 5 5
(\5 (\y -> 5 + y)) 5
(\y -> 5 + y) 5
(\5 -> 5 + 5)
5 + 5
10
```

Multi-argument lambdas work in exactly the same way (since they are the same).

```haskell
(\first last -> first ++ " " ++ last) "Tom" "Riddle"
(\"Tom" last -> "Tom" ++ " " ++ last) "Riddle"
(\"Tom" "Riddle" -> "Tom" ++ " " ++ "Riddle")
"Tom" ++ " " ++ "Riddle"
"Tom " ++ "Riddle"
"Tom Riddle"
```

So lambdas are evaluated, not so much as a series of steps, but rather as the mechanical replacing of symbols with inputs and then replacing the lambda with the result. This continues until you are only left with a value and can reduce no further, at which point the program is done!

## Using Lambdas for Tic Tac Toe 

A few math expressions and combining strings is all well and good, but how can we get from here to actually using lambdas to represent the game of tic tac toe? 

Indulge me for a moment as I get a bit philosophical, but isn't every action simply a mapping from some moment in the past to some moment in the future? Movies, as you probably know, are just a *ton* of pictures played rapidly, one after the other. The "action" of playing the movie is kinda just a mapping from some time to some picture. 0:00:00 maps to the start of the movie, 1:14:27 maps to some picture in the middle, and 11:23:59 maps to the last picture of the Lord of The Rings extended blue-ray.

In this same way, video games can be thought of as mappings to pictures. However, video games use a combination of time and user input to determine what picture to give you. When playing mario, the character remains still until the character presses a button. The game then takes that input and gives you a new picture with mario moved slightly. 

Board games are even easier. Any game of chess can be reproduced by just having a list of moves made by each player. Then "playing the game" is just mapping the moves to new boards.

For example: e4 e5, Qf3 f5, Bc4 fxe4, Qf7#

That's a five move checkmate >:]

We could write a program to take our moves (the data) and map them to boards (our function). This is the approach we can use in tac tic toe as well. For every move, we simply need to know who made the move and on what space they put that move. We can talk about specific spaces using coordinates like so:

```haskell
0,0 | 0,1 | 0,2
1,0 | 1,1 | 1,2
2,0 | 2,1 | 2,2
```

So our moves can be represented like this: (X, 0, 0), (O, 1, 2), ...

That's definitely a starting place, but tic tac toe is usually played by people so at some point we'll have to worry about actually getting the moves from humans. Luckily, we can leave that until the very end. For now, lets wrap up this section on functions with a few more concepts.

## Choices (A Bunch of Bool)

Sometimes, we want our mapping to change based on what is given to us. For example, if a customer has a discount, we want to give them the discounted prices instead of the normal ones. In order to make these kinds of choices, we'll need another type called `Bool`. There are only two possible values of a `Bool`: `True` or `False`. This allows us to say things like, "if it is true that the customer has a discount, then give the discounted prices, otherwise give the normal prices".

An application near and dear to our hearts at the moment is whether or not to end the game. If one of the players has one or the board is full, end the game, otherwise give the next state of the board. In this case, whether or not a player has won can be represented with a `Bool`. Either someone has won or they have not. Along the same lines, whether the board is full is equally state by a simple `True` or `False`.

### Case Expressions

In order to use values to make decisions we need something called a case expression. Case expressions allow you to check what the value used as the input to a lambda is and choose between several transformations.

```haskell
isZero = (\x -> case x of { 0 -> True; _ -> False; })
```

This lambda takes some Int x and checks whether or not it is 0 and gives a True if it is and for anything else (the _ symbol matches anything)it returns False.

This function would be useful in a situation like this:

```haskell
divide = (\x y -> case isZero y of { True -> 0; False -> x / y; })
```

While this isn't the best way to solve the problem of dividing by zero, we can put a bandaid on it like this.

Like multi-arguement functions there is a shorthand way to write case expressions that choose results based on a `Bool` value:

```haskell
(\x y -> if isZero y then 0 else x / y)
```

Which reads a better than the case expression.

### Basic Comparisons
- There are some built in functions which deal with generating True or False values. These "logic" and "comparison" operators are as follows
- `==` checks if the values are the same
- `>` and `<` have the same function as in math
- `>=` and `<=` mean greater than or equal to and lesser than or equal too
- `not` is a function that flips the boolean, True becomes False and visa versa
- `&&` means logical and, it takes two Booleans and, if they are both True, also returns true (anything else is False)
- `||` means logical or, it takes two Bool values and if either are True it will return True, False otherwise

## Using Comparisons for Tic Tac Toe

One of the problems we have to solve when making tic tac toe is going to be telling the difference between the symbol for `X` and the symbol for `O`. Knowing what we just learned, if we have a type which supports comparison, we can use functions like `==` to give us a `Bool` indicating if they are the same.

```haskell
"X" == "O"
```

This would reduce to `False` given both sides are different.

# Composition

Part of the power from functional programming is that lambdas, usually used to represent the actions of a program, can also be considered data just like String, Int, and Bool. We've already seen functions that can return other functions, so it goes without saying that perhaps we can do more with them.

Drawing again on the connection between human language and programming, we often use other words to change the effect or meaning of our verbs.

- He flew quickly. 
- She skipped joyfully. 
- They sighed with relief.

In Haskell we'll come to find many ways to modify our functions with *adverbs*, but for the moment, let's consider if we can chain a bunch of actions using something similar to "then".

In order to eat the noodles you'll have to heat *and then* stir *and then* blow on them.

If we have a bunch of actions we want to perform on some data, one after the other, and functions are data, then perhaps we don't have to write giant functions that do everything all at once. Perhaps we can write the little functions by themselves and then... add them?

```haskell
(\g f -> (\x -> g (f x)))
```

This is a function which implements *and then*. It takes two functions and makes a new function that takes the input, gives it to the second function and then the result to the first. This gives us the power to solve our problems one step at a time, and then combine the results when we are finished!

This idea is foundational to functional programming, so Haskell defines a special function for it called `.`

So

```haskell
eatNoodles :: Noodles -> Happiness
eatNoodles = \noodles -> blowOn (stir (heat noodles))
```

can be written as

```haskell
eatNoodles :: Noodles -> Happiness
eatNoodles = blowOn . stir . heat
```

- In order to apply these combined functions we can write something like this: `a . b . c . d $ input`
  - Here, everything to the left of `$` is the composition of a, b, c, and d

There are two ways to think about how evaluating this new "composed" function works.

Either the input value will flow left through each of these functions, getting transformed by each

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

Or the functions get combined and the input is added to last

```haskell
(\x -> x + 1) . (\x -> x + 2) . (\x -> x + 3) $ 5
(\x -> x + 1) . (\x -> (x + 3) + 2) $ 5
(\x -> ((x + 3) + 2) + 1) $ 5
(\5 -> ((5 + 3) + 2) + 1) 
(5 + 3) + 2) + 1 
(8 + 2) + 1 
10 + 1 
11
```

This can be done with parenthesis as well, but it often looks nicer to use `$` (which is just another function btw).

```haskell
$ = \f x -> f x
```

## Food for thought 

What happens if you don't give a function all of it's inputs? Remember that evaluation just substitutes the values one at a time. There is no reason stopping half way should break anything.

```haskell
(\x y -> x + y) 5
(\5 y -> 5 + y)
```

- Given that functions like `+` and `||` are still *just* functions, we can do the same for them

`(5 +) = ((+) 5) = (\x -> 5 + x)`

- If you have a bunch of functions applied to eachother (a (b (c (d input)))), you can rewrite it without the parenthesis using the ($) function. So `a $ b $ c $ d $ input`


# Creating New Types

### type

You can give aliases for other types.

```haskell
type Name = String
```

This is useful for when you wait to clarify what a value really represents. This is typically called "aliasing"

### newtype

If you want a stronger version of aliasing, then you can use a `newtype`

```haskell
newtype Name = Name String
```

Now you "wrapped" the String type, meaning that a `Name` is not equal to a `String`

### Product Types

A general way of grouping values together is a tuple

`(5,5)` is a tuple of two Integers.

Tuples are convient when you want to return multiple values from a function.

You can also create a named tuple with `data`

```haskell
data Coord = Coord Int Int
```

A `Coord` is just a grouping of two Integers, but it is not the same as `(Int, Int)`

You can use pattern matching to extract the values from product types.

```haskell
up :: Coord -> Coord
up = \(Coord x y) -> Coord x (y + 1)

resetY :: Coord -> Coord
resetY = \(Coord x _) -> Coord x 0
```

Just like in a case expression, `_` can be used to indicate values you don't care about and  match to anything.

If you want to name the items in your product type you can specify them like this

```haskell
data Person = Person { name :: String, age :: Int, height :: Double }
```

Now you can extract a value from a `Person` by using the functions for each value.

```haskell
over18 :: Person -> Bool
over18 = \p -> age p > 18
```

### Sum types

Sometimes you want to represent data which can be one of many things which might have different values.

```haskell
data Computer 
  = Desktop { ram :: Int, cpu :: String } 
  | Phone { screenSize :: Double, serviceProvider :: String}
```

So Computer is *either* a `Desktop` *or* a `Phone`.

In order to work with sum types you'll need to use case expressions because you can't know exactly which version of Computer it is beforehand.

```haskell
upgrade :: Computer -> Computer
upgrade = \c -> case c of {Desktop r c -> Desktop (r + 1) c; Phone s p -> Phone (s + 1) p;}
```

Sum types don't necessesarily need to wrap any values, they can also be used as a sort of "tag". You've already used a type like this if you've used `Bool` before.

```haskell
data Bool = True | False
```

While boolean values are primative types in other langauges, with sum types they can just be defined in the standard library. Another example could be to distinguish between Operating Systems.

```haskell
data OS = MACOS | WINDOWS | LINUX
```

Useful anytime you need to have several options and want to encode them directly instead of using something like `Int` or `String` to do so indirectly.

## Useful Shortcuts 

As your types become more complicated, you code becomes uglier and harder to read. Luckily, Haskell provides some useful shortcuts for dealing with complex data types.

```haskell
data Language = Cpp | Java | C | Python | Haskell

helloWorld = \l -> case l of { Cpp -> "cout << \"hello world\";"; Java -> "System.out.console.write(\"Hello world\");"; C -> "printf(\"hello world\");"; Python -> "print(\"hello world\")"; Haksell -> print \"Hello world\"";}
```

Goes wayyy of the page right? So one way to deal with this is to take advantage of the fact that we can move the lambda to the *other* side of the equal sign.

```haskell
id = \a -> a
```

is the same as

```haskell
id a = a
```

The nice thing about this is we can now pattern match on the value.

```haskell
isZero 0 = True
isZero _ = False
```

which is the same as writing

```haskell
isZero = \n -> case n of {0 -> True; _ -> False;}
```

So, rewriting our `helloWorld` example

```haskell
helloWorld Cpp = "cout << \"hello world\";"
helloWorld Java = "System.out.console.write(\"Hello world\");"
helloWorld C = "printf(\"hello world\");"
helloWorld Python = "print(\"hello world\")"
helloWorld Haksell = "print \"Hello world\""
```

In my opinion, this looks much nicer.

## Representing Moves For Tic Tac Toe 

Now that we understand how to make our own types, we can begin to describe the nouns of the game more exactly. For example, there are only three possible states that any space on the board could be: empty, taken by player `X`, or taken by player `O`. Since a space could be something *or* something else, we can represent that with a sum type.

```haskell
-- The symbols the board can be marked with
data Space = E | X | O deriving (Eq, Show)
```

Given a `Space` we can now represent a move!

```haskell
data Move = Move
  { moveRow::Int
  , moveCol::Int
  , moveSymbol::Space
  } deriving (Eq, Show)

```

These types are a start, but we'll need something a bit more expressive to be able to describe the entire board.

# Generic Types

Haskell's draws most of it's power, not from functions, but actually from its types. Instead of just being able to talk about combinations of existing types, we can actually create new types which don't really mind what they are combinations of. These types don't so much define data as they do a "structure" or a "shape". 

For example, it is possible to create a new type which wraps any type.

```haskell
data Wrap a = Wrap a
```

Not very exciting, except for the lowercase a on the left side of the equals.
This is a *type* parameter to the data. Just as functions are able take values,
types can also take parameters to create new types.

```haskell
wrappedInt :: Wrap Int
wrappedInt = Wrap 5

wrappedChar :: Wrap Char
wrappedChar = Wrap 'c'

wrappedBool :: Wrap Bool
wrappedBool = Wrap False
```

The `Wrap a` type isn't very useful since it really only acts like a super `newtype`, however there is a type similar to it which is useful.

### Maybe

```haskell
data Maybe a = Just a | Nothing
```

This type `Maybe` is a fancy `newtype` except it also adds on another value `Nothing`. In terms of what `Maybe` can represent, it's basically everything `a` can *in addition to* that `Nothing` value.

If we used `Maybe Bool` we can now represent three possible values: `True`, `False`, and `Nothing`. What does this mean exactly? Well consider a magic mirror that tells you, `True` or `False`, if you are the farest in all the land. If the magic face in the mirror was on break then you wouldn't be able to get an answer. We could represent this possibility with `Nothing`. It's neither `True` or `False` its just no answer.

In this way maybe is useful for encoding the possibility of failure within our programs. For example, what happens when you divide by zero? Perhaps we could use some pattern matching to "catch" the case when the user gives a 0.

```haskell
division x y :: Int -> Int -> Int
division x 0 = ???
division x y = x / y
```

There's a problem. What exactly do we return if they divide by zero? Maybe we could give some random value like `-1`, but that's a valid result for other values. And we can't just return *nothing* because in Haskell if the type says it returns an `Int` it *must* return an `Int`. No if, and, or but will save us from this.

Nothing to the rescue!

```haskell
division :: Int -> Int -> Maybe Int
division x 0 = Nothing
division x y = Just (x `div` y)
```

Now, there will never be any divison by zero. If zero is given for `y`, the result will be Nothing and the program can continue.

# Typeclasses 

Generic types are nice, but on their own they aren't all that interesting. Infact, there is very little we can actually *do* with generic types given that we don't know ahead of type what types are inside of them.

For example, there is only *one* possible definition for the function `id`.

```haskell
id :: a -> a
id x = x
```

That's it! There is literally nothing else we can do with the value given to `id` because we know nothing else about it. Generics like this are too abstract to do much with, so it would be nice if we could be a bit less ambiguous about the types we were using.

To achieve this we can define what is called a `typeclass`. Type classes are ways of ensure that generic types support certain functions before we can use them with a function. The simplest of these is the `Show` typeclass.

### Show

It is a useful property of any type to be convertable into a `String`. Whether we are trying to display values in a console or we are saving information to a file, types generally need to be stringable.

So lets make a type class for this property!

```haskell
class Show a where
  show :: a -> String
```

This defines the new type class. Any type which is a part of `Show` *must* define a version of the function `show` for that type.

```haskell
instance Show OS where
  show MACOS = "MACOS"
  show WINDOWS = "WINDOWS"
  show LINUX = "LINUX"
```

When you want to add a type to a type class, you must make an `instance` of all the functions for that class. Above we defined show for the `OS` type from earlier.

With very simple type classes like `Show`, the Haskell compiler is actually smart enough to define these instances for us.

```haskell
-- The symbols the board can be marked with
data Space = E | X | O deriving (Show)

data Move = Move
  { moveRow::Int
  , moveCol::Int
  , moveSymbol::Space
  } deriving (Show)

```

The `deriving` at the end of the type definition tells the compiler, "please figure out how to make this type an instance of `Show`". For most types we define, the compiler should be able to happily oblige.

### Eq

Another useful type class is `Eq` which means that values within that type can be compared.

```haskell
instance Eq Os where
  MACOS == MACOS = True
  WINDOWS == WINDOWS = True
  LINUX == LINUX = True
  _ == _ = False
``` 

In the above example, we define what it means for two `OS` values to be equal. And of course, the compiler can figure this one out as well.

Show now we have a way to convert our `Space` values into `String` values for displaying. In order to actually create the board though, we'll need one more type because we need the ability to talk about *many* `Space` values at the same time... or rather a list of `Spaces` values.

# Lists

Earlier we skirted over the fact that a `String` was a collection of `Char` values, but now it's time to be a bit more specific about what that means.

Given that collections are very handy tools to use when describing the world and the things that happen in it, we want some way to represent collections in our data. We can create collections or *lists* of things using a generic data type.

```haskell
data [a] = a : [a] | []
```

So a list is either an element with another list or an empty list.

```haskell
[1..]
```

This is the notation for defining a list of all numbers from 1 to infinity. The numbers aren't actually created until they are needed though. So if you only end up using the first 100, then it will only make the fist 100. Be careful with infinite lists because you might accidentally write something that uses all of them and then your program will never finish!

```haskell
take :: Int -> [a] -> [a]
```

Gives a list with only the first n elements of the list. A common tool to specify how much of an infinite list you actually want to use.

```haskell
drop :: Int -> [a] -> [a]
```

Gives a list without the first n elements.

```haskell
cycle :: [a] -> [a]
```

Given some list, create an infinite list with the inital list repeating.

## Food for thought
- Drop and iterate can be used to grab sections of a list
  - What are the first five letters after g?
  - take 5 . dropWhile ((/=) 'g') $ ['a'..]
- Iterate lets us generate arithmetic and geometric sequences
- You can use individual elements and simple rules to generate finite and infinte lists of values. Often times, this will be your method of setting up other functions.

# Transforming Lists

```haskell
map :: (a -> b) -> [a] -> [b]
```

Transforms each element of a list using some function.

```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

A new list with only the elements that satisfy the predicate.

```haskell
reverse :: [a] -> [a]
```

A list with the elements reversed.

```haskell
intersperse :: a -> [a] -> [a]
```

A list with some value placed between each element.

```haskell
transpose :: [[a]] -> [[a]]
```

Rotates a list of lists. A 3x2 list becomes a 2x3.

## Food for thought
- map and filter can be used "query" lists
- List comprehensions are shorthands for this. Most programs will consist of generating a basic list of values, then transforming that list into something more suited for your particular problem.

# Folding Lists

```haskell
foldr :: (b -> a -> b) -> b -> [a] -> b
```

Using some function and an inital value, combine all the elements of a list.

```haskell
concat :: [[a]] -> [a]
```

Given a list of lists, create a new list with all the elements from each list.

```haskell
concatMap :: (a -> [b]) -> [a] -> [b]
```

Apply some function which generates a list to each element and then concatenate the results.

```haskell
and :: [Bool] -> Bool
```

Given a list of Bool values, fold the list using &&.

```haskell
or :: [Bool] -> Bool
```

Given a list of Bool values, fold the list using ||.

```haskell
any :: (a -> Bool) -> [a] -> Bool
```

Whether any value of some list satisfies some predicate.

```haskell
all :: (a -> Bool) -> [a] -> Bool
```

Whether all elements of a list satisfy a predicate.

```haskell
mapM_ :: (a -> IO b) -> [a] -> IO ()
```

Perform an `IO` action using every element of a list.

## Food For Thought 

Folds can collapse a list down to a single result, but they can also be thought of as "sequencing" events, as in forM_ and mapM_. So lists are not only a means of holding "values", but also a means of structuring the execution of IO actions.

## Creating The Board

```haskell
boardSize :: Int
-- ^ The length and width of the board
boardSize = 3

startingRow :: Int -> [Move]
-- ^ Defines the default starting place for the game
startingRow n = map (\i -> Move n i E) [1..boardSize]

startingBoard :: [[Move]]
startingBoard = map startingRow [1..boardSize]

boardSpot :: [Move] -> Int -> Int -> Space
-- ^ Checks if a spot on the board has been claimed
boardSpot moves row col
  | Move row col X `elem` moves = X
  | Move row col O `elem` moves = O
  | otherwise = E

filledBoard :: [Move] -> [[Space]]
-- ^ Takes a list of moves and creates a board with those moves made
filledBoard moves = map (map (\m -> boardSpot moves (moveRow m) (moveCol m))) startingBoard

showBoard :: [[Space]] -> [String]
-- ^ Converts a board into a list of rows
showBoard = map (concatMap show)

printBoard :: [Move] -> IO ()
-- ^ Prints the board one row at a time
printBoard = mapM_ putStrLn . showBoard . filledBoard
```

## Checking for Winner

Given our knowledge of `List` and `Bool`, we can now check to see if someone has won the game yet.

```haskell
lineWin :: [Space] -> Bool
-- ^ Check if a player has won on a give line
lineWin [] = False
lineWin (E:_) = False
lineWin (first:rest) = (rest /= emptyLine) && all (first ==) rest
  where emptyLine = replicate (length rest) E

rowWin :: [[Space]] -> Bool
-- ^ Check if any line has been won
rowWin = any lineWin

colWin :: [[Space]] -> Bool
-- ^ Flip the board and use rowWin
colWin = rowWin . transpose

-- [1,2,3] 0
-- [4,5,6] 1
-- [7,8,9] 2
-- ([1,2,3] !! 0), ([4,5,6] !! 1), ([7,8,9] !! 2)

diagonalWin :: [[Space]] -> Bool
-- ^ Create lists of the diagonals and check them with line win
diagonalWin board = lineWin leftDiag || lineWin rightDiag
  where interval = take boardSize [0..]
        leftDiag = zipWith (!!) board interval
        rightDiag = zipWith (!!) board $ reverse interval

gameOver :: Space -> [Move] -> Space
-- ^ Checks if the game is over
gameOver player moves = if winner then player else E
  where spaces = filledBoard moves
        winner = or [rowWin spaces, colWin spaces, diagonalWin spaces]
```

## User Input

```haskell
validMove :: String -> String -> Space -> [Move]-> Maybe Move
-- ^ Defines a valid move given by the player
validMove r c player moves = do
  row <- Text.readMaybe r:: Maybe Int
  col <- Text.readMaybe c:: Maybe Int
  let move = Move row col player
      valid n = n >= 1 && n <= boardSize in
    if valid row && valid col && (move `notElem` moves)
      then Just move
      else Nothing

getPlayerMove :: Space -> [Move] -> IO Move
-- ^ The action of getting they player's move
getPlayerMove player moves = do
  putStr "Enter Row: "
  row <- getLine
  putStr "Enter Col: "
  col <- getLine
  case validMove row col player moves of
    Just move -> pure move
    Nothing -> getPlayerMove player moves
```

## Putting It All Together

```haskell
turn :: Space -> Space -> [Move] -> IO ([Move], Space)
-- ^ Represents a single player turn
turn player E moves = do
  printBoard moves
  move <- getPlayerMove player moves
  pure $ gameOver player (move:moves)
turn _ winner moves = pure (moves, winner)

playerTurns :: [Space]
-- ^ Creates a list of alternating X and O turns
playerTurns = take (boardSize^2) $ cycle [X, O]

game :: [Move] -> IO ([Move], Space)
-- ^ Assigns the turns a player, composes the turns
game = foldr1 (<=<) (map turn playerTurns) E

main :: IO ()
-- ^ evaluates the game starting with no moves and prints the winner
main = do
  (_, winner) <- game [] 
  print . (++) "Winner is " . show $ winner
```
