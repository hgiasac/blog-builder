# PureScript GADTs Alternative - Recap

GADTs ("Generalized Algebraic Data Types") provide an explicit instantiation of the ADT as the type instantiation of their return value. This gives you power to define type safe data structures with more advanced type behavior. Unfortunately, in the time of this post, latest version or PureScript (0.12) hasn't supported GADTs yet.

However, we can overcome the limit with alternative methods. This post is just a recap from what I researched.

### The Problem 

With GADTs extension, we could write a little eDSL for arithmetic and comparison expressions something like this:

```haskell
data Expr a where
  Val :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Mult :: Expr Int -> Expr Int -> Expr Int
  Equal :: Expr Int -> Expr Int -> Expr Bool
  Not :: Expr Bool -> Expr Bool

eval :: Expr a -> a
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Mult x y) = eval x * eval y
eval (Equal x y) = eval x == eval y
eval (Not x) = not (eval x)
```

Without GADTs it seems like this eval function would be impossible to write, since we wouldn’t have the information to determine the result type that matching on a data constructor of a GADT gives us.

## Leibniz equality

> This is recap from [Approximating GADTs in PureScript][1] 

> Two types are equal if they are equal in all contexts.

### The solution

We can’t directly encode type equalities using the type system in PureScript, but we can take advantage of [“Leibniz equality”][2] and encode it in a value instead:

```purescript 
newtype Leibniz a b = Leibniz (forall f. f a -> f b)

infix 4 type Leibniz as ~

symm :: forall a b. (a ~ b) -> (b ~ a)
symm = ...

coerce :: forall a b. (a ~ b) -> a -> b
coerce = ...
```

`(a ~ b)` is a Leibniz value, with `a` is a variable that is identical with some type `b`. This allows us to `coerce` one type to another.

The `symm` function is used when we have a type equality that we need to “turn around” so that we can coerce in either direction based on a provided Leibniz value. This is possible thanks to the fact that equality relations are symmetric (for every a and b, if a = b then b = a), and is where the name `symm` comes from.

Go back to the example. In the GADT-defined Expr, the final part of the type annotation for each data constructor, ... -> Expr SomeType, is essentially saying “a ~ SomeType for this constructor”. 

When a constructor of a GADT is pattern-matched, the type-checker gains knowledge of this equality and then lets us return a value of the appropriate type, rather than being stuck with the rigid type variable a.

```purescript
data Expr a
  = Add (Expr Int) (Expr Int) (a ~ Int)
  | Mult (Expr Int) (Expr Int) (a ~ Int)
  | Equal (Expr Int) (Expr Int) (a ~ Boolean)
  | Not (Expr Boolean) (a ~ Boolean)
  | Val Int (a ~ Int)
```

We can now write `eval` using the `coerce` function to safely cast the result type back to a in each case:

```purescript 
coerceSymm :: forall a b. (a ~ b) -> b -> a
coerceSymm = coerce <<< symm

eval :: forall a. Expr a -> a
eval (Val x proof) = coerceSymm proof x
eval (Add x y proof) = coerceSymm proof (eval x + eval y)
eval (Mult x y proof) = coerceSymm proof (eval x * eval y)
eval (Equal x y proof) = coerceSymm proof (eval x == eval y)
eval (Not x proof) = coerceSymm proof (not (eval x))
```

The only thing we haven’t covered is how to actually construct a Leibniz value in the first place. What possible function could we provide that satisfies `forall f. f a -> f b` where `a` equals `b`? The identity function! Leibniz also has a Category instance, so actually we can just use `identity` directly:

```purescript
val :: Int -> Expr Int
val x = Val x identity

add :: Expr Int -> Expr Int -> Expr Int
add x y = Add x y identity

mult :: Expr Int -> Expr Int -> Expr Int
mult x y = Mult x y identity

equal :: Expr Int -> Expr Int -> Expr Boolean
equal x y = Equal x y identity

not :: Expr Boolean -> Expr Boolean
not x = Not x identity
```

And finally, we can run our original example cases again:

```purescript
> eval (not (equal (mult (val 10) (val 1)) (add (val 0) (val 1))))
true
```

### Limitations

As the title of original post, [Gary Burgess](https://twitter.com/gb_r) called it `Approximating GADTs`. This method can't solve all cases, especially in more complicated eDSLs. For example in polymorphism rank N-type DSL:

```haskell
data Expr a where
    Val     :: Int                     -> Expr Int
    Lambda  :: (Expr a -> Expr b)      -> Expr (Expr a -> Expr b)
    Apply   :: Expr (Expr a -> Expr b) -> Expr a -> Expr b
    Add     :: Expr Int -> Expr Int    -> Expr Int
```

Since PureScript doesn't support existential types in AST, this method is useless.

```purescript
data ExprL a = Val Int (a ~ Int) 
              | Add (ExprL Int) (ExprL Int) (a ~ Int)
              | Lambda (ExprL b -> ExprL c) (a ~ (ExprL b -> ExprL c)) -- can't define existential type
              | Apply (forall b c. ExprL (ExprL b -> ExprL c)) (forall b. ExprL b) (forall c. a ~ c) -- nope
```

## Tagless Final comes to rescue

> In the final approach, object language terms are represented as expressions built from a small set of combinators, which are  ordinary  functions  rather  than  data  constructors.  The  values  of  these  expressions give denotations of the corresponding object terms. An object term is hence represented not by its abstract syntax but by its denotation in a semantic domain. Abstracting over the domain gives us a family of interpretations.
> [Typed Tagless Final Interpreters - Oleg Kiselyov][3]

### The solution

In Tagless Final approach, instead of declaring AST data type directly, we use type class as a set of operations:

```purescript
class LambdaSym repr where
  val :: Int -> repr Int
  lambda :: forall a b. (repr a -> repr b) -> repr (a -> b)
  apply :: forall a b. repr (a -> b) -> repr a -> repr b
  add :: repr Int -> repr Int -> repr Int
```

It accepts all interpreters that implement those operations

```purescript
data R a = R a

eval :: forall a. R a -> a
eval (R a) = a

instance lambdaSymmR :: LambdaSym R where
  val a = R a
  lambda f = R (\a -> unR $ f (R a))
  apply (R f) (R a) = R (f a)
  add (R x) (R y) = R $ x + y
```

The syntax is similar to Leibniz approach 

```purescript
testR :: forall repr. LambdaSym repr => repr Int
testR = apply (lambda (\x -> add (val x) (val x))) (val 10)

result :: Int 
result = eval testR 
```

Tagless Final has more advantages than GADTs and Leibniz approaches:
- Flexible, easy to extend the language with new operations.
- Easier to implement
- Requires less boilerplate. 
- Combine different parts of the DSL very easily, without change the existed code

```purescript
class MultiplySymm repr where
  mult :: repr Int -> repr Int -> repr Int 

testR' :: forall repr. LambdaSym repr => MultiplySymm repr => repr Int
testR' = apply (lambda (\x -> mult x x)) (val 10)
```

### Limitations

From what I researched, because Haskell hasn't supported impredicative polymorphism. That means that we can’t specialize an existing data type to hold a polymorphic value like this:

```haskell
Maybe (LambdaSym repr => repr Int)
```

Fortunately, PureScript support impredicative polymorphism. We no need to worry about it.

```purescript
type L = forall repr. Maybe (LambdaSym repr => repr Int)

testR':: L
testR' = Nothing
```

## Conclusion 

Compared to Leibniz, or even GADTs, Tagless Final style has many advantage as well as easy to grasp. It is wide adoption in functional programming world. Tagless Final style is a good alternative to GADTs 

## References

1. [Approximating GADTs in PureScript - Gary Burgess][1]
2. [Typed Tagless Final Interpreters - Oleg Kiselyov][2]

[1]: http://code.slipthrough.net/2016/08/10/approximating-gadts-in-purescript/
[2]: https://en.wikipedia.org/wiki/Identity_of_indiscernibles
[3]: http://okmij.org/ftp/tagless-final/course/lecture.pdf
