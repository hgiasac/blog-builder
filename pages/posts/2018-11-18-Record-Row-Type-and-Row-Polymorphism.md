# Record, Row Types an Row Polymorphism

Record, or object in another programming languages, is usually defined as a list of product `name => value` pairs. The most common syntax to access record's properties is dot `.`

```javascript
const obj = { x: 1, y: 2 }

console.log(obj.x) // 1
```

In Scala, we use `class` or `case class`, because object is singleton
In Haskell, there is no "record type", it is only the syntactic sugar to a constructor type with functions that access it's fields

```haskell
data Person = Person { name :: String, age :: Int }

-- syntactic sugar to
data Person = Person String Int 

name :: Person :: String 
name (Person name _) = name

age :: Person :: Int 
age (Person _ age) = age

-- so, you can create a record in both ways
person :: Person 
person = Person { name = "Mike", age = 1 }
-- or 
person = Person "Mike" 1
```

Therefore, the most common issue in Haskell is namespacing for record field names. That means you can't define `Person { name :: String }` and `Cat { name :: String }` in same module. Function `name` will be duplicated. Moreover, Record in Haskell isn't extensible and polymorphism

PureScript is inspired of Haskell, however Record mechanism is different. It's constructor takes a row of concrete types. Same as JavaScript, we use dot to access properties.

```haskell
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

```haskell
-- closed row
type ClosedPersonRow = ( name :: String, age :: Number )
-- opened row
type OpenedPersonRow r = ( name :: String, age :: Number | r )

type Person r = Record (OpenedPersonRow r)
```

You have noticed polymorphic variable `r`, right? In other words, Person accepts any record which has properties name and age, and any other record properties. 

```haskell
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


[1]: https://purescript-simple-json.readthedocs.io/en/latest/inferred-record-types.html
[2]: https://github.com/purescript/documentation/blob/master/language/Types.md
