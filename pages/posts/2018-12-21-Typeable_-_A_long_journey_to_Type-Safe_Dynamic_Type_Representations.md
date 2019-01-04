# Typeable - A long journey to type-safe dynamic type representation

Haskell has grown and changed amazingly in recent years, with great contribution from community and industrial adoption. There are many new ideas and changes in new GHC versions, as well as breaking changes. The most influent library is `base` package, which contains the Standard Haskell Prelude, and its support libraries, and a large collection of useful libraries ranging from data structures to parsing combinators and debugging utilities. ([hackage][1])

[Typeable][2] is a module in `base` package. Maybe you already know, the `Typeable` class is used to create runtime type information for arbitrary types. Explicitly, `Typeable` is a unique [Fingerprint][3] (or hash) of type coupled with a coercion. This module has big influence in `Data.Dynamic`, `Data.Data` module as well as generic programming libraries. This post gives you a short story about the innovation of `Typeable` along with GHC extensions.

## A long time ago...

`Typeable` was originally a part of `Dynamic`, which is the solution for Dynamic typing in Statically typed language. In real world, there are programming situations we need to deal with data whose type cannot be determined at compile time, for example, fetch data from API, parse JSON string, query from database... 

Since mid-1960s, early programming languages (Algol-68, Pascal, Simula-67) used similar technique of `Dynamic`. Until 1989, `Dynamic` and `typecase` term was introduced in ["Dynamic Typing in a Statically Typed Language"][4] paper, with lambda calculus notation.

In Haskell, from what I saw in [git.haskell.org][5], the first commit was in June 28, 2001 by [Simon Marlow][7]. However, I'm not sure this proof is correct. Maybe the module was implemented before that. Unfortunately I can't find another reliable source. In this version, `Typeable` and `Dynamic` were in same module. In [July 24 2003][6], `Typeable` was moved to `Data.Typeable` package. 

In the early stage of Haskell, `TypeRep` was constructed of `TyCon` and child `TypeRep`s. `TyCon` hold `Key` as an unique `HashTable`, and a `String` to determine type name at user level. 

```haskell
data TypeRep = TypeRep !Key TyCon [TypeRep] 

data TyCon = TyCon !Key String

mkTyCon :: String  -> TyCon 

boolTc :: TyCon
boolTc = mkTyCon "Bool"

class Typeable a where
  typeOf :: a -> TypeRep

> show $ typeOf (True :: Bool) -- Bool

```

FFI C macros can help generating instances, with some exceptions, such as `Tuple`. `undefined` was used as an alternative for Proxy. `unsafeCoerce` was used anywhere for coercing types.

```haskell

INSTANCE_TYPEABLE0(Bool,boolTc,"Bool")

tup2Tc :: TyCon
tup2Tc = mkTyCon ","

instance (Typeable a, Typeable b) => Typeable (a,b) where
  typeOf tu = mkAppTy tup2Tc [typeOf ((undefined :: (a,b) -> a) tu),
                              typeOf ((undefined :: (a,b) -> b) tu)]
 
tup3Tc :: TyCon
tup3Tc = mkTyCon ",,"

instance ( Typeable a , Typeable b , Typeable c) => Typeable (a,b,c) where
  ...

tup4Tc :: TyCon
tup4Tc = mkTyCon ",,," -- too much boilerplate
```

## From Key to Fingerprint

`TyCon` was stored in unsafe IORef cache, which internally kept a `HashTable` mapping `String`s to `Int`s. So that each `TyCon` could be given a unique Int for fast comparison, the `String` has to be unique across all types in the program. However, derived instances of typeable used the qualified original name (e.g. `GHC.Types.Int`) which is not necessarily unique, is non-portable, and exposes implementation details.

```haskell
newtype Key = Key Int deriving( Eq )

data KeyPr = KeyPr !Key !Key deriving( Eq )

data Cache = Cache { next_key :: !(IORef Key),
                     tc_tbl   :: !(HT.HashTable String Key),
                     ap_tbl   :: !(HT.HashTable KeyPr Key) 
                   }
```

In July 2011, [Simon Marlow][7] replaced `Key` with [Fingerprint]() - , and moved `TyCon` as well as `TypeRep` internally. The fields of TyCon are not exposed via the public API. Together the three fields `tyConPackage`, `tyConModule` and `tyConName` uniquely identify a `TyCon`, and the Fingerprint is a hash of the concatenation of these three Strings (so no more internal cache to map strings to unique IDs). This implementation is also easier for GHC to generate derived instances

```haskell
data TyCon = TyCon {
   tyConHash    :: {-# UNPACK #-} !Fingerprint,
   tyConPackage :: String,
   tyConModule  :: String,
   tyConName    :: String
}

data TypeRep = TypeRep {-# UNPACK #-} !Fingerprint TyCon [TypeRep]
```

## Merge all the things  

With the rise of Type-level programming, GHC compiler is more and more better. Many boilerplate codes were refactored and optimized in safe and well-performed. `Typeable` also took advantage of new GHC features:

### PolyKinds

- [PolyKinds][9] extension has been implemented since [GHC 7.4.1 (February 2012)][10]. Until [GHC 7.6.1 (September 2012)][14] release, Polymorphic kinds and data promotion were fully implemented and supported features.
- November 2012, [José Pedro Magalhães @dreixel](https://twitter.com/dreixel) [adapted poly-kinded into Typeable][11], then released in [GHC 7.8.1 (April 2014) - base-4.7.0.0][12]

Before Poly-kinds, there are many duplicated code to support variant for N-ary type constructors. To represent type constructors with kind
`* -> *`, such as `Maybe` or `[]`, we could create a separate type class, called `Typeable1`. However, this approach is ugly and inflexible. What about tuples? Do we need a `Typeable2`, `Typeable3`... for them?

```haskell
class Typeable (t :: *) where
  typeOf :: t -> TypeRep

class Typeable1 (t :: * -> *) where
  typeOf1 :: t a -> TypeRep

...

-- The maximum number of parameters was 7 
class Typeable7 (t :: * -> * -> * -> * -> * -> * -> * -> *) where
  typeOf7 :: t a b c d e f g -> TypeRep
```

With kind polymorphism we can write:

```haskell
class Typeable a where
  typeRep :: proxy a -> TypeRep

typeOf :: forall a. Typeable a => a -> TypeRep
  typeOf _ = typeRep (Proxy :: Proxy a)
```

We  have  generalized  in  two  ways  here.  
- First, `Typeable` gets `a`  polymorphic kind: `forall a. a -> Constraint`, so that it can be used for types of any kind. 
- Second, we need some way to generalize the argument of `typeRep` , which we have done via a poly-kinded data type Proxy: 

```haskell
data Proxy a = Proxy`
```

The idea is that, say `typeRep (Proxy :: Proxy Int)` will return the type representation for `Int`, while `typeRep (Proxy :: Proxy Maybe)` will do the same for `Maybe`. The proxy argument carries no information—the type has only one, nullary constructor and is only present so that the programmer can express the type at which to invoke `typeRep`. Because there are no constraints on the kind of `a`, it is safe to assign `Proxy` the polymorphic kind `forall a. a -> *`.

### TypeLevelReasoning

[Richard A. Eisenberg][13] also proposed and implemented [TypeLevelReasoning][14]. New module `Data.Type.Equality` was born.  The idea is defines the type of equality witnesses for two types of any kind `k`:

```haskell
data a :~: b where
  Refl :: a :~: a
```
Pattern matching on this generalized algebraic datatype (GADT) allows GHC to discover the equality between two types. If `a :=: b` is inhabited by some terminating value, then the type `a` is the same as the type `b`. To use this equality in practice, pattern-match on the `a :=: b` to get out the `Refl` constructor:

```haskell
coerce :: (a :~: b) -> a -> b
  coerce Refl x = x
```
We pattern-match on `Refl`. This exposes the fact that `a` and `b` must be the same. Then, GHC happily uses `x` of type `a` in a context expecting something of type `b`.

With this, we can remove unsafe hack when convert Proxy:

```haskell
-- no equality
cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x = r
  where
    r = if typeOf x == typeOf (fromJust r)
        then Just $ unsafeCoerce x
        else Nothing

-- with equality
eqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~: b)
eqT = if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b)
      then Just $ unsafeCoerce Refl
      else Nothing
 
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x
  | Just Refl <- ta `eqT` tb = Just x
  | otherwise                 = Nothing
  where
    ta = typeRep :: TypeRep a
    tb = typeRep :: TypeRep b
```

### And more...

This release also added is the `AutoDeriveTypeable` language extension, which will automatically derive `Typeable` for all types and classes declared in that module.

The update could caused breaking changes, although backwards compatibility interfaces were kept. José Pedro Magalhães also backed up old codes into `Data.OldTypeable` module, which was removed later in [GHC 7.10.1 - base-4.8.0.0](http://hackage.haskell.org/package/base-4.8.0.0/changelog)

- Since GHC 8.2, GHC has supported type-indexed type representations. `Data.Typeable` provides type representations which are qualified over this index. To keep `Typeable` backwards compatibility, the interface of this module is similar to old releases. `Type.Reflection` module is available for the type-indexed interface.

## Safer and more expressive type representations

With the motivation of distribution programming, mainly focusing on [Cloud Haskell][23], new challenges occurred: Typeable wasn't safe enough.

### Open world's challenge

[Cloud Haskell][23] is an Erlang-style library for programming a distributed system in Haskell. It is based on the message-passing model of Erlang, but with additional advantages that stem from Haskell’s purity, types, and monads. If you are from Scala land, Cloud Haskell has similar features with [Akka](https://akka.io), which use Actor model for communicating across processes. However, Cloud Haskell also supports thread in same machine. You can conveniently do things the old way.

Because the information that is transmitted between machines by network must be in bit, the data must be `Serializable`. This ensures two properties: The data can be encoded and decoded to binary fore and back, and can produce a `TypeRep` that captures the item’s type.

```haskell
class (Typeable a, Binary a) => Serializable a

class Binary a where
  put :: t -> PutM ()
  get :: Get t

class Typeable a where
  typeOf :: a -> TypeRep

encode :: a -> ByteString
decode :: ByteString -> Maybe (a, ByteString)
```

To guarantee that the code is then applied to appropriately typed values, the receiving node must perform a dynamic type test. That way, even if the code pointer was corrupted in transit, by accident or malice, the receiving node will be type-sound. You can think of it like this: a decoder is simply a parser for the bits in the `ByteString`, so a decoder for `Int` can fail to parse a full `Int` (returning `Nothing`), but it can't return a non-Int. A simple way to do this is to serialize a code pointer as a key into a static pointer table containing Dynamic values ([RemoteTable][24]). When receiving code pointer with key `k`, the recipient can lookup up entry `k` in the table, find a `Dynamic`, and check that it has the expected type.

To do that, `Typeable` needed to evolve further.

`TypeRep` was not indexed, so there was no connection between the `TypeRep` stored in a  `Dynamic` and the corresponding value. Indeed, accessing the `typeRep` required a proxy argument to specify the type that should be represented.

```haskell
data TypeRep 

data Typeable a where
  typeRep :: proxy a -> TypeRep

data Dynamic where
  Dyn :: TypeRep -> a -> Dynamic 

data Proxy a = Proxy
```

Because there is no connection between types and their representations, this implementation of `Dynamic` requires `unsafeCoerce`. For example, here is the old `fromDynamic`:

```haskell
fromDynamic :: forall d. Typeable d => Dynamic -> Maybe d 
fromDynamic (Dyn trx x)
  | typeRep (Proxy :: Proxy d) == trx = Just (unsafeCoerce x)
  | otherwise = Nothing
```

Client code is un-trusted. The current design (GHC 7.8) couldn't hide all such uses of `unsafeCoerce` from the user. If they could write a `Typeable` instance, they must use `unsafeCoerce`, and defeat type safety. So only GHC is allowed write `Typeable` instances. 

### Kind-indexed GADTs

The key to our approach is our type-indexed type representation `TypeRep`. But what is a type-indexed type representation? That is, the index in a type-indexed type representation is itself the represented type. For example:

- The representation of `Int` is the value of type `TypeRep Int`.
– The representation of `Bool` is the value of type `TypeRep Bool`.
- And so on.

The idea of kind-indexed is originally from GADTs. For example, we consider designing a GADT for closed type representations:

```haskell
data TyRep :: * -> * where
  TyInt  :: TyRep Int
  TyBool :: TyRep Bool
```

GADTs differ from ordinary algebraic data types in that they allow each data constructor to constrain the type parameters to the datatype. For example, the `TyInt` constructor requires that the single parameter to `TyRep` be `Int`.

We can use type representations for type-indexed programming a simple example liked computing a default element for each type.

```haskell
zero :: forall a. TyRep a -> a
zero TyInt  = 0          -- ‘a’ must be Int
zero TyBool = False      -- ‘a’ must be Bool
```

This code pattern matches the type representation to determine what value to return. Because of the nonuniform type index, pattern matching recovers the identity of the type variable `a`. 

- In the first case, because the data constructor is `TyInt`, this parameter must be `Int`, so `0` can be returned. 
- In the second case, the parameter `a` must be equal to `Bool`, so returning `False` is well-typed.

However, the GADT above can only be used to represent types of kind `*`. To represent type constructors with kind `* -> *`, such as `Maybe` or `[]`, we could create a separate datatype, perhaps called `TyRep1`, `TyRep2`,... Kind polymorphism which allows data types to be parameterized by kind variables as well as type variables, could be the solution. However, it is not enough to unify the representations for `TyRep`. the type representation (shown below) should constrain its kind parameter.

```haskell
data TyRep (a :: k) where
  TyInt :: TyRep Int
  TyBool :: TyRep Bool
  TyMaybe :: TyRep Maybe
  TyApp :: TyRep b -> TyRep c -> TyRep (b c)
```

This `TyRep` type takes two parameters, a kind `k` and a type of that kind (not named in the kind annotation). The data constructors constrain
`k` to a concrete kind. For the example to be well-formed, `TyInt` must constrain the kind parameter to *. Similarly, `TyMaybe` requires the kind parameter to be `* -> *`. We call this example a kind-indexed GADT because the datatype is indexed by both kind and type information.

Pattern matching with this datatype refines kinds as well as types—determining whether a type is of the form `TyApp` makes new kind and type equalities available. For example, consider the zero function extended with a default value of the `Maybe` type.

```haskell
zero :: forall (a :: *). TyRep a -> a
zero TyInt = 0
zero TyBool = False
zero TyApp (TyMaybe _) = Nothing
```

In the last case, the `TyApp` pattern introduces the kind variable `k`, the type variables `b :: k -> *` and `c :: k`, and the type equality `a ∼ b c`. The `TyMaybe` pattern adds the kind equality `k ~ *` and type equality `b ∼ Maybe`. Combining the equality, we can show that `Maybe c`, the type of `Nothing`, is well-kinded and equal to `a`. 

With this design, we also enable type decomposition feature. Finally, new `TypeRep` will be:

```haskell
data TypeRep (a :: k) where
  TrTyCon :: !Fingerprint -> TyCon -> TypeRep k -> TypeRep a
  TrApp   :: !Fingerprint -> TypeRep a -> TypeRep b -> TypeRep (a b)

data TyCon = TyCon { tc_module :: Module, tc_name :: String }
data Module = Module { mod_pkg :: String, mod_name :: String }

```

The current implementation of `TypeRep` allows constant-time comparison based on fingerprints. To support this in the new scheme we would want to add a fingerprint to every TypeRep node. But we would not want clients to see those fingerprints. 

The `TyCon` type, which is a runtime representation of the “identity” of a type constructor, is now silently generates a binding for a suitable instance for every datatype declaration by GHC. For example, for `Maybe` GHC will generate:

```haskell
$tcMaybe :: TyCon 
$tcMaybe = TyCon { tc_module = Module { mod_pkg = "base"
                                      , mod_name = "Data.Maybe"
                                      }
                 , tc_name = "Maybe"
                 }
```

The name `$tcMaybe` is not directly available to the programmer. Instead (this is the second piece of built-in support), GHC’s type-constraint solver has special behavior for `Typeable` constraints, as follows.

To solve `Typeable(t1 t2)`, GHC simply solves `Typeable t1` and `Typeable t2`, and combines the results with `TrApp`. To solve `Typeable T` where `T` is a type constructor, the solver uses `TrTyCon`. The first argument of `TrTyCon` is straight-forward: it is the (runtime representation of the) type constructor itself, e.g `$tcMaybe`.

But `TrTyCon` also stores the representation of the kind of this very constructor, of type `TypeRep k`. Recording the kind representations is important, otherwise we would not be able to distinguish, say, `Proxy :: * -> *` from `Proxy :: (* -> *) -> *`, where Proxy has a polymorphic kind `(Proxy :: forall k. k -> *)`. We do not support direct representations of kind-polymorphic constructors like `Proxy`, rather
`TrTyCon` encodes the instantiation of a kind-polymorphic constructor (such as Proxy).

Notice that `TrTyCon` is fundamentally insecure: you could use it to build a `TypeRep t` for any `t` whatsoever. That is why we do not expose the representation of `TypeRep` to the programmer. Instead the part of GHC’s Typeable solver that builds `TrTyCon` applications is part of GHC’s trusted code base. 

`TypeRep` is abstract, and thus we don't use proxy to determine the `TypeRep` value anymore:

```haskell
-- before
class Typeable a where
  typeRep :: proxy a -> TypeRep

-- after
data Typeable a where
  typeRep :: TypeRep
```

### Kind equalities

We also need to recompute representation equality function `eqT`. It is easy, just need to compare equality between 2 `TypeRep` fingerprint:

```haskell
data (a :: k1) :~~: (a :: k2) where
  HRefl :: forall (a :: k). a :~~: a

eqT :: forall k1 k2 (a :: k1) (b :: k2)
        . TypeRep a -> TypeRep b -> Maybe (a :~~: b)
eqT a b 
  | typeRepFingerprint a == typeRepFingerprint b = Just (unsafeCoerce Refl)
  | otherwise = Nothing
```

It is critical that this function returns `(:~~:)`, not `(:~:)`. GHC can't compile. This is because `TyCon`s exist at many different kinds. For example, `Int` is at kind `*`, and `Maybe` is at kind `* -> *`. Thus, when comparing two `TyCon` representations for equality, we want to learn whether the types and the kinds are equal. If we used type equalities `(:~:)` here, the `eqTypeRep` function could be used only when we know, from some other source, that the kinds are equal. 

[Richard A. Eisenberg][13] proposed and implemented [kind heterogeneous equalities][26] (2013 - 2015). It enable new, useful features such as kind-indexed GADTs, promoted GADTs and kind families. This extension was experiment in GHC 8.0.1, then was provided in `Data.Type.Equality` module.

The restriction above exists because GHC reasons about only type equality, never kind equality. The solution to all of these problems is simple to state: merge the concepts of type and kind. If types and kinds are the same, then we surely have kind equalities. In order to overcome those challenges, it has been necessary to augment GHC’s internal language, `System FC`. This is beyond the scope of this post. If you need to dig into detail, let's [read this paper][21].

`fromDynamic` turns out like this:

```haskell
fromDynamic :: forall d. Typeable d => Dynamic -> Maybe d
fromDynamic (Dyn (ra :: TypeRep a) (x :: a))
  = case eqT ra (typeRep :: TypeRep d) of 
      Nothing -> Nothing
      Just HRefl -> Just x
```

We use `eqT` to compare the two `TypeRep`s, and pattern-match on `HRefl`, so that in the second case alternative we know that `a` and `d` are equal, so we can return `Just x` where a value of type `Maybe d` is needed. More generally, `eqT` allows to implement type-safe cast, a useful operation in its own right.

### Decomposing polykinds representations

So far, we have discussed type representations for only types of kind `*`. The only operation we have provided over `TypeRep` is `eqT`, which compares two type representations for equality. Does `(,)` which has kind `(* -> *)` too have a `TypeRep`? For example, how can we decompose the type representation, to check that it indeed represents a pair, and extract its first component? 

```haskell
dynFst :: Dynamic -> Maybe Dynamic
dynFst (Dyn rpab x)
  = ...
```

Of course it must. Since types in Haskell are built via a sequence of type applications (much like how an expression applying a function to multiple arguments is built with several nested term applications), the natural dual is to provide a way to decompose type applications. Let's take a look at `TrApp` definition:

```haskell
data TypeRep a where  
  TrCon :: !Fingerprint -> TyCon -> TypeRep k -> TypeRep a
  TrApp :: !Fingerprint -> TypeRep a -> TypeRep b -> TypeRep (a b)
```

`TrApp` allows us to observe the structure of types and expose the type equalities it has discovered to the type checker. Now we can implement
`dynFst`:

```haskell
dynFst :: Dynamic -> Maybe Dynamic
dynFst (Dyn (TrApp _ rpa rb) x) 
  = case rpa of 
      TrApp rp ra -> case eqT rp (typeRep :: TypeRep (,)) of
                      Just Refl -> Just $ Dyn ra (fst x)
                      Nothing -> Nothing
      _ -> Nothing
dynFst _ = Nothing
```

We check that the `TypeRep` of x is of form `(,) a b` by decomposing it twice. Then we must check that `rp`, the `TypeRep` of the function part of this application, is indeed the pair type constructor `(,)`; we can do that using `eqT`. These three GADT pattern matches combine to tell the type checker that the type of `x`, which began life in the `(Dyn rpab x)` pattern match as an existentially-quantified type variable, is indeed a pair type `(a, b)`. So we can safely apply `fst` to `x`, to get a result whose type representation `ra` we have in hand.

The code is simple enough, but the type checker has to work remarkably hard behind the scenes to prove that it is sound. Let us take a closer
look with kind signatures added:

```haskell
data TypeRep a where 
  TrApp :: forall k1 k2 (a :: k1 -> k2) (b :: k1) 
         . !Fingerprint -> TypeRep a -> TypeRep b -> TypeRep (a b)
```

Note that `k1`, the kind of `b` is *existentially* bound in this data structure, meaning that it does not appear in the kind of the result type `(a b)`. We know the result kind of the type application but there is no way to know the kinds of the sub-components.

With kind polymorphism in mind, let’s add some type annotations to see what existential variables are introduced in `dynFst`:

```haskell

dynFst :: Dynamic -> Maybe Dynamic
-- dynFst (Dyn (rpab :: TypeRep pab) (x :: pab))
dynFst (Dyn (TrApp _ (rpa :: TypeRep pa) (rb :: TypeRep b)) (x :: pab)) 
  -- introduces kind k2, and types pa :: k2 -> *; b :: k2
  = case rpa of 
      TrApp (rp :: TypeRep p) (ra :: TypeRep a) -> 
        -- introduces kind k1, and types p :: k1 -> k2 -> *, a :: k1
        case eqT rp (typeRep :: TypeRep (,)) of
          Just Refl -> Just $ Dyn ra (fst x)
          -- introduces p ~ (,) and (k1 -> k2 -> *) ~ (* -> * -> *)
          Nothing -> Nothing
      _ -> Nothing
dynFst _ = Nothing
```

Focus on the arguments to the call to `eqT` in the third line. We know that:

- rp      :: TypeRep p    and p   :: k1 -> k2 -> *
- typeRep :: TypeRep (,)  and (,) :: *  -> *  -> *

So `eqT` must compare the `TypeRep`s for two types of different kinds; if the runtime test succeeds, we know not only that `p ~ (,)`, but also that `(k1 ~ *)` and `(k2 ~ *)`. That is, the pattern match on `Refl` GADT constructor brings local kind equalities into scope, as well as type equalities.


### Better Pattern matching with PatternSynonyms

The code above is ugly. Moreover, we don't want to expose `TypeRep` constructor to users. Earliest solution is returning another GADT:

```haskell
data AppResult t where
  App :: TypeRep a -> TypeRep b -> AppResult (a b)

splitApp :: TypeRep a -> Maybe (AppResult a)
splitApp (TrApp _ ra rb) = Just $ App ra rb
splitApp _               = Nothing

dynFst :: Dynamic -> Maybe Dynamic
dynFst (Dyn rpab x) = do 
  App rpa rb <- splitApp rpab
  App rp ra <- splitApp rpa
  Refl <- eqT rp (typeRep :: TypeRep (,))
  return $ Dyn ra (fst x)
```

However, we can make it better with `PatternSynonyms` extension which was provided since GHC 7.8.

> Pattern synonyms enable giving names to parametrized pattern schemes. They can also be thought of as abstract constructors that don’t have a bearing on data representation.
> [GHC User's Guide][29]

```haskell
pattern App :: forall k2 (t :: k2). ()
            => forall k1 (a :: k1 -> k2) (b :: k1). (t ~ a b)
            => TypeRep a -> TypeRep b -> TypeRep t
pattern App f x <- TrApp _ f x
  where App f x = mkTrApp f x


dynFst :: Dynamic -> Maybe Dynamic
dynFst (Dyn (App (App rp ra) rb) x) = do
  Refl <- eqT rp (typeRep :: TypeRep (,))
  return $ Dyn ra (fst x)
dynFst _                            = Nothing
```

With this extension, you can not only hide the representation of the datatype, but also use it in pattern matching. Our code is much cleaner.

### Decompose function type, TypeInType and Dependent Types

We also need to implement `dynApply`, which applies a function Dynamic to an argument Dynamic. It is necessary in real-world application, e.g, send an object with a function type, say `Bool -> Int`, over the network.

```haskell
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dyn rf f) (Dyn rx x) = ?
```

In theory, We can use `TrApp` and `App` pattern to construct and decompose function type `(->)` too. The definition of this function is fairly
straightforward:

```haskell
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dyn (TrApp (TrApp tr targ) tres) fun) (Dyn targ' arg)
  | Just HRefl <- eqT tr (typeRep :: TypeRep (->))
  , Just HRefl <- eqT targ targ'
  = Just (Dyn tres (fun arg))
dynApply _ _ = Nothing
```

However, because functions are quite ubiquitous, we should define another constructor for the sake of efficiency:

```haskell
data TypeRep (a :: k) where 
  TrFun :: TypeRep a -> TypeRep b -> TypeRep (a -> b)
```

This definition wasn't compiled before GHC 8.0. First, `TrApp` and `TrFun` is ambiguous that can be solved with explicit quantification. Second, GHC can't know that kind `TypeRep :: k -> *` is identical with `* -> *`.

Kind equality extends the idea of kind polymorphism by declaring that types and kinds are indeed one and the same. Nothing within GHC distinguishes between types and kinds. Another way of thinking about this is that the type `Bool` and the “promoted kind” `'Bool` are actually identical. 

> One simplification allowed by combining types and kinds is that the type of `Type` is just `Type`. It is true that the `Type :: Type` axiom can lead to non-termination, but this is not a problem in GHC, as we already have other means of non-terminating programs in both types and expressions. This decision (among many, many others) does mean that despite the expressiveness of GHC’s type system, a “proof” you write in Haskell is not an irrefutable mathematical proof. GHC promises only partial correctness, that if your programs compile and run to completion, their results indeed have the types assigned. It makes no claim about programs that do not finish in a finite amount of time.
> [GHC User's Guide][31]

To enable `(* ::*)` axiom, you have to enable `TypeInType` extension, which is a deprecated alias of `PolyKinds`, `DataKinds` and `KindSignatures`. Its functionality has been integrated into these other extensions. With this extension, GHC can know that `k ~ *`, and the code can compile.

```haskell
data TypeRep (a :: k) where
  TrTyCon :: !Fingerprint -> TyCon -> TypeRep k -> TypeRep (a :: k)
  TrApp   :: forall k1 k2 (a :: k1 -> k2) (b :: k1). 
          !Fingerprint 
          -> TypeRep (a :: k1 -> k2)
          -> TypeRep (b :: k1)
          -> TypeRep (a b)
  TrFun   :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                     (a :: TYPE r1) (b :: TYPE r2).
          !Fingerprint 
          -> TypeRep a
          -> TypeRep b
          -> TypeRep (a -> b)
```

### Levity Polymorphism

You may notice, there are `RuntimeRep` and `TYPE` kind signatures in TrFun constructor. They relates to Levity Polymorphism, which is implemented in GHC version 8.0.1, released early 2016.

In brief, most of types we use everyday (Int, Bool, AST, ...) are boxed value, which is represented by a pointer into the heap. The main advantage of boxed types are supporting polymorphism. The disadvantage is slow performance. Most polymorphic languages also support some form of unboxed primitive values that are represented not by a pointer but by the value itself. In Haskell, unboxed types are denoted with `MagicHash` suffix.

```haskell
{-# LANGUAGE MagicHash #-}

f :: Int# -> Int#
```

Haskell also categorizes types into `lifted`, and `unlifted`. Lifted types is one that is lazy. It is considered `lifted` because it has one extra element beyond the usual ones, representing a non-terminating computation. For example, Haskell’s `Bool` type is lifted, meaning that three `Bool`s are possible: `True`, `False` ,and `⊥`. An `unlifted` type, on the other hand, is strict. The element `⊥` does not exist in an unlifted type.

Because Haskell represents lazy computation as thunks at runtime, all lifted types must also be boxed. However, it is possible to have boxed, unlifted types.

+ Lifted - Boxed: `Int`, `Bool`,...
+ Unlifted - Boxed: `ByteArray#`
+ Unlifted - Unboxed: `Int#`, `Bool#`

Given these unboxed values, the boxed versions can be defined in Haskell itself; GHC does not treat them specially. For example:

```haskell
data Char = C# Char#
data Int = I# Int#

plusInt :: Int -> Int -> Int 
plusInt (I# i1) (I# i2) = I# (i1 +# i2)
```

Here `Int` is an ordinary algebraic data type, with one data constructor `I#`, that has one field of type `Int#`. The function `plusInt` simply
pattern matches on its arguments, fetches their contents (`i1` and `i2`, both of type `Int#`), adds them using `(+#)`, and boxes the result with `I#`.

The issue is, like many other compilers for a polymorphic language, GHC assumes that a value of polymorphic type, such as `x :: a` s represented uniformly by a heap pointer, or lifted type. The compiler adopts `The Instantiation Principle`: 

> You cannot instantiate a polymorphic type variable with an unlifted type.

That is tiresome for programmers, but in return they get solid performance guarantees.

How can the compiler implement the instantiation principle? For example, how does it even know if a type is unlifted? By kinds, much the same way that terms are classified by types. `Type`, or `*` kind which we use every day is lifted. In contrast, `#` is a new kind that classifies unlifted types, e.g: `Int#`, `Bool#`.

In default, polymorphism functions assume kind of parameters is `Type`. If we attempt to instantiate it at type `Float# :: #`, we will get a
kind error because `Type` and `#` are different kinds. The function arrow type `(->)` is the same. It is just a binary type constructor with kind:

```haskell
(->) :: Type -> Type -> Type
```

But now we have a serious problem: a function over unlifted types, such as `sumTo# :: Int# -> Int# -> Int#`, becomes ill-kinded!. 

For many years its “solution” was to support a sub-kinding relation. That is, GHC had a kind `OpenKind`, a super-kind of both `Type` and `#`. We could then say that:

```haskell
(->) :: OpenKind -> OpenKind -> Type 
```

However, there are drawbacks. The combination of type inference, polymorphism, and sub-typing, is problematic. And indeed GHC’s implementation of type inference was riddled with awkward and unprincipled special cases caused by sub-kinding. Moreover, The kind `OpenKind` would embarrassingly appear in error messages.

Levity polymorphism bring new idea: replace sub-kinding with kind polymorphism. New primitive type-level constant, `TYPE :: RuntimeRep -> Type` is introduced with the following supporting definitions:

```haskell
data RuntimeRep = VecRep VecCount VecElem   -- ^ a SIMD vector type
                | TupleRep [RuntimeRep]     -- ^ An unboxed tuple of the given reps
                | SumRep [RuntimeRep]       -- ^ An unboxed sum of the given reps
                | LiftedRep       -- ^ lifted; represented by a pointer
                | UnliftedRep     -- ^ unlifted; represented by a pointer
                | IntRep          -- ^ signed, word-sized value
                | WordRep         -- ^ unsigned, word-sized value
                | Int64Rep        -- ^ signed, 64-bit value (on 32-bit only)
                | Word64Rep       -- ^ unsigned, 64-bit value (on 32-bit only)
                | AddrRep         -- ^ A pointer, but /not/ to a Haskell value
                | FloatRep        -- ^ a 32-bit floating point number
                | DoubleRep       -- ^ a 64-bit floating point number

type Type = TYPE 'LiftedRep 
```

RuntimeRep is a type that describes the runtime representation of values of a type. `Type`, the kind that classifies the types of values, was
previously treated as primitive, but now becomes a synonym for `TYPE Lifted`, where `Lifted :: RuntimeRep`. It is easiest to see how these
definitions work using examples:

```haskell
Int :: Type
Int :: TYPE `LiftedRep -- Expanding Type
Int# :: TYPE 'IntRep
Float# :: TYPE 'FloatRep
(Int, Bool) :: Type
Maybe Int :: Type
Maybe :: Type -> Type
```

Any type that classifies values, whether boxed or unboxed, lifted or unlifted, has kind `TYPE r` for some `r :: RuntimeRep`. The type
`RuntimeRep` tells us the runtime representation of values of that type. This datatype encodes the choice of runtime value.

We can now give proper types to `(->)`, same as `TrFun`. This enables polymorphism for both lifted and unlifted types.

```haskell
import GHC.Exts

(->) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
      . TYPE r1 -> TYPE r2 -> Type
```

**Note**: Unboxed and Levity Polymorphism types are imported in `GHC.Exts` module.

If you are more curious about Levity polymorphism, let's take a look at [original paper][15].

### Compare TypeReps

It is sometimes necessary to use type representations in the key of a map. For example, [Shake][32] uses a map keyed on type representations to look up class instances (dictionaries) at runtime; these instances define class operations for the types of data stored in a collection of `Dynamic`s. Storing the class operations once per type, instead of with each `Dynamic` package, is much more efficient.

More specifically, we would like to implement the following interface:

```haskell
data TyMap 
empty :: TyMap 
insert :: Typeable a => a -> TyMap -> TyMap
lookup :: Typeable a => TyMap -> Maybe a
```

But how should we implement these type-indexed maps? One option is to use `HashMap`. We can define the typed-map as a map between the type representation and a dynamic value.

```haskell
data SomeTypeRep where
  SomeTypeRep :: forall k (a :: k). !(TypeRep a) -> SomeTypeRep

type TyMap = HashMap SomeTypeRep Dynamic
```

Notice that we must wrap the `TypeRep` key in an existential `SomeTypeRep`, otherwise all the keys would be for the same type, which would rather defeat the purpose! The `insert` and `lookup` functions can then use `toDynamic` and `fromDynamic` to ensure that the right type of value is stored with each key.

```haskell
import Data.HashMap.Strict as Map

insert :: Typeable a => a -> TyMap -> TyMap 
insert x = Map.insert (SomeTypeRep (typeRep :: TypeRep a)) (toDynamic x)

lookup :: Typeable a => TyMap -> Maybe a
lookup = fromDynamic <=< Map.lookup (SomeTypeRep (typeRep :: TypeRep a))
```

Because `Map` family uses balanced binary trees to achieve efficient lookup, `SomeTypeRep` must be an instance of `Ord`. This is straightforward, since `TypeRep` use fingerprint for unique hash, it can be compared too. 

```haskell
instance Ord SomeTypeRep where
  SomeTypeRep a `compare` SomeTypeRep b =
    typeRepFingerprint a `compare` typeRepFingerprint b 
```

Notice that we cannot make an instance for `Ord (TypeRep a)`: if we compare two values both of type `TypeRep t`, following the signature of
compare, they should always be equal!

### Other Changes

+ Type-indexed type representation interface is in `Type.Reflection` module.` Data.Typeable` provides type representations which are qualified over this index, providing an interface very similar to the `Typeable` notion seen in previous releases for backward compatibility.
+ `Data.Typeable.TypeRep` and `Type.Reflection.TypeRep` are different. `Data.Typeable.TypeRep` is alias of `SomeTypeRep`  
+ `TypeRep` definition is replaced with record, to easier to extend parameters
```haskell
data TypeRep (a :: k) where
    TrType :: TypeRep Type
    TrTyCon :: { trTyConFingerprint :: {-# UNPACK #-} !Fingerprint
               , trTyCon :: !TyCon
               , trKindVars :: [SomeTypeRep]
               , trTyConKind :: !(TypeRep k) 
               }  
            -> TypeRep (a :: k)
            
    TrApp   :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
               { 
                 trAppFingerprint :: {-# UNPACK #-} !Fingerprint
               , trAppFun :: !(TypeRep (a :: k1 -> k2))
               , trAppArg :: !(TypeRep (b :: k1))
               , trAppKind :: !(TypeRep k2) }
            -> TypeRep (a b)

    TrFun   :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                      (a :: TYPE r1) (b :: TYPE r2).
               {
                 trFunFingerprint :: {-# UNPACK #-} !Fingerprint
               , trFunArg :: !(TypeRep a)
               , trFunRes :: !(TypeRep b) }
            -> TypeRep (a -> b)
```
+ The kind of the TypeRep in each `TrTyCon` and `TrApp` constructor is cached. This is necessary to ensure that typeRepKind (which is used, at least, in deserialization and dynApply) is cheap, because calculating the kind of type constructor and nested type applications is pricy,
+ We need to be able to represent `TypeRep Type`. This is a bit tricky because `typeRepKind (typeRep @Type) = typeRep @Type`, so if we actually cache the `TypeRep` of the kind of `Type`, we will have a loop. One simple way to do this is to make the cached kind fields lazy and allow `TypeRep Type` to be cyclical.
  
## Limitation and unexplored future

Our interface does not support representations of polymorphic types, such as `TypeRep (∀ a. a -> a)`. Although plausible, supporting those in our setting brings in a whole new range of design decisions that are as of yet unexplored (e.g. higher-order representations vs. de-Bruijn?). Furthermore, it requires the language to support impredicative polymorphism (the ability to instantiate quantified variables with polymorphic types, for instance the `a` variable in `TypeRep a` or `Typeable a`), which GHC currently does not. Finally, representations of polymorphic types have implications on semantics and possibly parametricity.

Similarly, constructors with polymorphic kinds would require impredicative kind polymorphism. A representation of type `TypeRep (Proxy :: ∀ kp. kp -> *)` would require the kind parameter `k` of `TypeRep (a :: k)` to be instantiated to the polymorphic kind `∀ kp. kp -> *`. Type inference for impredicative kind polymorphism is no easier than for impredicative type polymorphism and we have thus excluded this possibility.

## Summary 

`Typeable` take a long journey for definition, design, refactor and redesign, from runtime term level to type-safe. However, the journey doesn't stop here. GHC is still growing, better and safer, with innovation and contribution of brilliant researchers. It motivates us to study and do great software and bring toward the industry.

## Acknowledgment

Thanks to Richard A. Eisenberg and his team for great research and contribution. This post take many reference in their papers. 

## Appendix

- The complete code in "A reflection on types" paper: [link][33]
- Homogeneous equality ~ Type equality: (`a :~: b`)
- Heterogeneous equality ~ Kind equality: (`a :~~: b`)
- Universal quantification: `forall a. a -> a`
- Existentially quantified type: `forall a. Show a => a -> String`

## References

- [Dynamic Typing in a Statically Typed Language - Martin Abadi, Luca Cardelli, Benjamin Pierce, Gordon Plotkin (1989)][4]
- [Kind polymorphism - GHC Language Features. Chapter 7][9]
- [Scrap your boilerplate: a practical approach to generic programming - Ralf Lämmel, Simon Peyton Jones (January 2003)][20]
- [A Lightweight Implementation of Generics and Dynamics - James Cheney, Ralf Hinze (October 3, 2002)][21]
- [Giving Haskell a Promotion - Brent A. Yorgey, Stephanie Weirich, Julien Cretin, Simon Peyton Jones, Dimitrios Vytiniotis, Jose Pedro Magalhaes (2012)][17]
- [Who invented proxy passing and when? - Stackoverflow][19]
- [What is Levity polymorphism - Stackoverflow][18] 
- [Levity polymorphism (extended version) - Richard A. Eisenberg, Simon Peyton Jones (2017)][15]
- [TypeLevelReasoning Proposal - Richard A. Eisenberg][14]
- [An overabundance of equality: Implementing kind equalities into Haskell - Richard A. Eisenberg (September 22, 2015)][26]
- [Typed reflection in Haskell - Simon Peyton Jones, Stephanie Weirich, Richard A. Eisenberg, Dimitrios Vytiniotis - Proc Philip Wadler's 60th birthday Festschrift, Edinburgh, April 2016][8]
- [Safer and more expressive type representations - Haskell Trac Wiki][22]
- [Types-safe Distributed Haskell - Haskell Trac Wiki][25]
- [System FC with Explicit Kind Equality][27]
- [A reflection on types][8]
- [What does the exclamation mark mean in a Haskell declaration? - Stackoverflow][28]
- [Haskell Git Repository][16]
- [Towards Haskell in the Cloud - Jeff Epstein, Andrew Black, Simon Peyton Jones (September 2011)][30]
- [GHC User's Guide][31]
  
[1]: http://hackage.haskell.org/package/base-4.12.0.0
[2]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Typeable.html
[3]: http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Fingerprint.html
[4]: http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-47.pdf
[5]: http://git.haskell.org/packages/base.git/history/HEAD:/Data/Dynamic.hs
[6]: http://git.haskell.org/packages/base.git/history/HEAD:/Data/Typeable.hs
[7]: https://simonmar.github.io
[8]: https://www.microsoft.com/en-us/research/publication/typed-reflection-in-haskell
[9]: https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html
[10]: https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/release-7-4-1.html
[11]: http://hauptwerk.blogspot.com/2012/11/coming-soon-in-ghc-head-poly-kinded.html
[12]: https://downloads.haskell.org/~ghc/7.8.1/docs/html/users_guide/release-7-8-1.html
[13]: https://cs.brynmawr.edu/~rae/
[14]: https://ghc.haskell.org/trac/ghc/wiki/TypeLevelReasoning
[15]: https://cs.brynmawr.edu/~rae/papers/2017/levity/levity-extended.pdf
[16]: http://git.haskell.org
[17]: http://dreixel.net/research/pdf/ghp.pdf
[18]: https://stackoverflow.com/a/35320729
[19]: https://stackoverflow.com/questions/37261593/who-invented-proxy-passing-and-when
[20]: https://www.microsoft.com/en-us/research/publication/scrap-your-boilerplate-a-practical-approach-to-generic-programming
[21]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.19.7113&rep=rep1&type=pdf
[22]: https://ghc.haskell.org/trac/ghc/wiki/Typeable
[23]: https://wiki.haskell.org/Cloud_Haskell
[24]: https://hackage.haskell.org/package/distributed-static-0.3.8/docs/Control-Distributed-Static.html#v:unstatic
[25]: https://ghc.haskell.org/trac/ghc/wiki/DistributedHaskell
[26]: https://cs.brynmawr.edu/~rae/papers/2015/equalities/equalities.pdf
[27]: https://cs.brynmawr.edu/~rae/papers/2013/fckinds/fckinds.pdf
[28]: https://stackoverflow.com/questions/993112/what-does-the-exclamation-mark-mean-in-a-haskell-declaration
[29]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-PatternSynonyms
[30]: https://www.microsoft.com/en-us/research/publication/towards-haskell-cloud/
[31]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overview-of-type-in-type
[32]: http://hackage.haskell.org/package/shake-0.17.3/docs/Development-Shake.html
[33]: https://gist.github.com/hgiasac/1072f9f97b57732ac0040c122ce7f41b
