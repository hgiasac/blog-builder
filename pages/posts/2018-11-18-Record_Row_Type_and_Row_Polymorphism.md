# Record, Row Types an Row Polymorphism

Record, or object in another programming languages, is usually defined as a list of product `name => value` pairs. The most common syntax to access record's properties is dot `.`

```javascript
const obj = { x: 1, y: 2 }

console.log(obj.x) // 1
```

In Scala, we use `class` or `case class`, because object is singleton
In Haskell, there is no "record type", it is only the syntactic sugar to a constructor type with functions that access it's fields

```purescript
data Person = Person { name :: String, age :: Int }

-- syntactic sugar to
data Person = Person String Int 

name :: Person -> String 
name (Person name _) = name

age :: Person -> Int 
age (Person _ age) = age

-- so, you can create a record in both ways
person :: Person 
person = Person { name = "Mike", age = 1 }
-- or 
person = Person "Mike" 1
```

Therefore, the most common issue in Haskell is namespacing for record field names. That means you can't define `Person { name :: String }` and `Cat { name :: String }` in same module. Function `name` will be duplicated. Moreover, Record in Haskell isn't extensible and polymorphism

PureScript is inspired of Haskell, however Record mechanism is different. It's constructor takes a row of concrete types. Same as JavaScript, we use dot to access properties.

```purescript
data Record :: # Type -> Type

type Person = Record (name :: String, age :: Number)
-- or syntactic sugar with curly braces  
type Person = { name :: String, age :: Number }

person :: Person 
person = { name: "Mike", age: 1 }

log person.name
```

The fun here is Row Types, which make Record more flexible and overcome the issue of Haskell


## Row Types and Row Polymorphism

Row Types is well explained in PureScript [documentation][2]:

> A row of types represents an unordered collection of named types, with duplicates. Duplicate labels have their types collected together in order, as if in a NonEmptyList. This means that, conceptually, a row can be thought of as a type-level Map Label (NonEmptyList Type).
> Rows are not of kind Type: they have kind # k for some kind k, and so rows cannot exist as a value. Rather, rows can be used in type signatures to define record types or other type where labelled, unordered types are useful.

Therefore, Row Types can be extensible and polymorphism. It is denoted similar to Record, replaced with closed brackets. To denote an open row, separate the specified terms from a row variable by a pipe. It defines arguments for Record's constructor

```purescript
-- closed row
type ClosedPersonRow = ( name :: String, age :: Number )
-- opened row
type OpenedPersonRow r = ( name :: String, age :: Number | r )

type Person r = Record (OpenedPersonRow r)
```

You have noticed polymorphic variable `r`, right? In other words, Person accepts any record which has properties name and age, and any other record properties. 

```purescript
type Pet = Record { species :: String | Person }

introduce :: { name :: String, age :: String | r } -> String 
introduce { name, age } = name <> ", " <> show age <> " years old"

introduce { name: "Doraemon", age: 3, species: "dog" }
-- Doraemon, 3 years old

introduce { name: "Doraemon" }
-- Error: Type of expression lacks required label age.

```

However, the last one can't be compiled, because property `age` is missing.

It doesn't stop here. The real fun of Row Types is Type-Level Programming, which is introduced in the next section 

## Prim.Row 

PureScript compiler also provides automatically solved type classes for working with row types. They help the code type-safe, generic, reduce duplicated codes, yet take advantage of compiler's power. You can read the documentation at [Pursuit][3]. However, you will be easier to grasp with simple examples.

[purescript-record][4] includes functions for working with records and polymorphic labels. Under the hood, these functions use Foreign Function Interface (FFI) from JavaScript object, and Row Constraints.

`union` function implement `Union` class. It returns record `r3` including properties of both records `r1`, `r2`.

```purescript
class Union (left :: # Type) (right :: # Type) (union :: # Type) 
  | left right -> union, right union -> left, union left -> right

union :: forall r1 r2 r3. Union r1 r2 r3 => { | r1 } -> { | r2 } -> { | r3 }

union { x: 1, y: "y" } { y: 2, z: true }
 :: { x :: Int, y :: String, y :: Int, z :: Boolean }
```

In record, labels can duplicated. To keep labels unique, use `nub`, implement of `Nub` constraint

```purescript
class Nub (original :: # Type) (nubbed :: # Type) | original -> nubbed

nub :: forall r1 r2. Nub r1 r2 => { | r1 } -> { | r2 }

nub $ union { x: 1, y: "y" } { y: 2, z: true }
-- { x: 1, y: "y", z: true }
```

**Note**: 
- Nub is left hand priority. `y :: String` will be kept instead of right `y :: Int`. You have to think opposite if you come from JavaScript land.
- `nub` is used for polymorphism records. In theory, record labels can be duplicated, but not in compiler. That means if you type `{ x: 1, y: "y", y: 2, z: true }`, it can't compile. 

`merge` is the combination of `Union` and `Nub`:

```purescript
merge :: forall r1 r2 r3 r4. Union r1 r2 r3 => Nub r3 r4 => { | r1 } -> { | r2 } -> { | r4 }

merge { x: 1, y: "y" } { y: 2, z: true }
-- { x: 1, y: "y", z: true }
```

`class Cons` constraint helps the compiler know that, there is a property with label `l`. It is useful if you want to access a property in any record without know its constructor. `get` and `set` functions are most common use case of `Cons`

```purescript

class Cons (label :: Symbol) (a :: Type) (tail :: # Type) (row :: # Type) 
  | label a tail -> row, label row -> a tail

get :: forall r r' l a
  . IsSymbol l 
  => Cons l a r' r 
  => SProxy l 
  -> { | r } 
  -> a

get (SProxy :: SProxy "x") { x: 1, y: "y", z: true }
-- 1

set :: forall r1 r2 r l a b
  . IsSymbol l 
  => Cons l a r r1 
  => Cons l b r r2 
  => SProxy l 
  -> b 
  -> { | r1 } 
  -> { | r2 }


set (SProxy :: SProxy "x") "x" { x: 1, y: "y", z: true }
-- { x: "x", y: "y", z: true }
```

`class Lacks` is the opposite of `Cons`, a given Symbol label not existing in row. You may think of `insert` and `delete` functions that add/remove a property with label `l` from input record

```purescript
class Lacks (label :: Symbol) (row :: # Type) 

insert :: forall r1 r2 l a
  . IsSymbol l 
  => Lacks l r1 
  => Cons l a r1 r2 
  => SProxy l -> a -> { | r1 } -> { | r2 }

insert (SProxy :: SProxy "x") "x" { y: "y", z: true }
-- { x: "x", y: "y", z: true }

delete :: forall r1 r2 l a
  . IsSymbol l 
  => Lacks l r1 
  => Cons l a r1 r2 
  => SProxy l -> { | r2 } -> { | r1 }

delete (SProxy :: SProxy "x") { x: "x", y: "y", z: true }
-- { y: "y", z: true }
```

You may have an idea "We can insert and delete without Lacks constraint". Yes, in theory we can. However, label can be duplicated. Inserting same property many times can cause unexpected results. `Lacks` constraint keep label unique.

## When to use Row Constraints?

Unless you are developing libraries, or generics and type-level programming, you don't need them. Row polymorphism and [purescript-record][4] library is good enough for daily use. However, if you need to write FFI and reuse existed JavaScript library, row types is very useful. The most common use is optional record. 

For example, [default React components's props record][5] are many optional attributes. You don't really need to construct all fields.

```purescript
type SharedProps specific =
  -- | `key` is not really a DOM attribute - React intercepts it
  ( key               :: String

  , about             :: String
  , acceptCharset     :: String
  , accessKey         :: String
  , allowFullScreen   :: Boolean
  ...
  | specific
  )
```

However, if you make Props as parameter to create JSX element. You must fill all label values. It is really boring and useless.

```purescript
p :: Record SharedProps_p -> JSX 

p { key: "", about: "", acceptCharset: "", accessKey: "", ... } -- uhh, too long
```

Before PureScript support row types, the popular idea is using Array Props to define React properties (Haskell DOM libraries is the same idea). It is still good. The flaw is attributes can be duplicated.

Since Row Constraints appearance, the issue has easily been solved.

```purescript
p
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_p)
  => Record attrs
-> JSX

p { className: "text-center" } [ text "hello world"]
```
[purescript-react-basic][6] takes advantage of row types to make React simple, type-safe and more.

One common case is default values. In JavaScript, you will do it with:

```javascript
const defaultValues = { x: 0, y: "" }

const result = (input) => Object.assign(defaultValues, input);
```

In PureScript, the code will be 

```purescript

type ObjectRow r = ( x :: Int, y :: String )
type OpenRow r = ( x :: Int, y :: String | r )

result :: 
  forall r1 r2
  . Union r1 ObjectRow (OpenRow r2)
  => Nub (OpenRow r2) ObjectRow
  => Record r1 -> Record OpenRow
result r1 = merge r1 { x: 0, y: "" }
```

However, most of time we don't need to do with row types, if input directly

```purescript
f :: Record OpenRow -> String

f (merge { x: 1 } defaultValues)
-- or 
f defaultValues { x = 1 }
```

## Does Haskell support Row Types?

Yes. There is library [row-types][7]. It is self-explain, right? However, there are another extensions that works with Record too. Row Types are better in PureScript because it is easy to do with FFI and JavaScript Object. The critical point is performance and complexity. We can't say which one is the best.

## Conclusion

Because PureScript is a transpiler, working with FFI is unavoidable. Row Types help us to work with records easily in polymorphic way, Row Types are not only method to work with Record. [purescript-variant] provide similar functions. However, there are more fun things to do with Row family. `RowList` is the cool feature that support generic methodology to have fun with record. [Justin Woo][9] - a brilliant PureScript contributor - published a list of tutorials and talks about this topic. You can read his posts [here][10]

[1]: https://purescript-simple-json.readthedocs.io/en/latest/inferred-record-types.html
[2]: https://github.com/purescript/documentation/blob/master/language/Types.md
[3]: https://pursuit.purescript.org/builtins/docs/Prim.Row
[4]: https://github.com/purescript/purescript-record
[5]: https://github.com/lumihq/purescript-react-basic/blob/master/src/React/Basic/DOM/Internal.purs
[6]: https://github.com/lumihq/purescript-react-basic
[7]: http://hackage.haskell.org/package/row-types
[8]: https://github.com/natefaubion/purescript-variant
[9]: https://twitter.com/jusrin00
[10]: https://github.com/justinwoo/awesome-rowlist
