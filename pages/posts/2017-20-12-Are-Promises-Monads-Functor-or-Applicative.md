# Are Promises Monads, Functor or Applicative
 
If we have ever programmed with JavaScript, we all know about Promises. The Promise object abstracts the future value of an asynchronous operation. It can be the eventual completion (or failure) of asynchronous result

Promises is very popular today. Many imperative programing languages adapt it. However, when we learn about functional programming, especially Haskell, sometimes we wonder that Promises are monads?

Haskell is near to mathematic. Each type class is a mathematic object. If we want to know a instance is a type of Functor, Monad or Applicative,we need to prove if the target is satisfy the laws.  

Because each language has another mechanism to implement Promises, Promises can be different. In first section, I will talk about JavaScript. 

## Laws

Before go to the detail, let's a look at those laws. Note, p ≡ q simply means that you can replace p with q and vice-versa, and the behavior of your program will not change: p and q are equivalent.

### Functor

```haskell
fmap id ≡ id -- Identity law

fmap (g . f) ≡ fmap g . fmap f -- Associativity

```

### Monad

```haskell
return a >>= f ≡ f a -- Left identity

m >>= return ≡ m -- Right identity

(m >>= f) >>= g ≡	 m >>= (\x -> f x >>= g) -- Associativity
```

## JavaScript

### Are Promises Functor? 

The analogous Promise function for `fmap` is `then`. The constructor is `Promise.resolve`.

```javascript
// id :: a -> a
const id = (a) => a

// fmap id ≡ id -- functor law
const lp = Promise.resolve(1).then(id)
const rp = id(Promise.resolve(1))

Promise.all([lp, rp]).then(([a, b]) => console.log(a === b))
// true

```

Promise seems satisfy Identity law. We can go next with Associativity law.

```javascript
// fmap (g . f) ≡ fmap g . fmap f -- Associativity
const f = (a) => a * 2;
const g = (a) => a * 3;

Promise.all([
  Promise.resolve(1).then(f).then(g),
  Promise.resolve(1).then(a => g(f(a))),
]);
// true
```

Hooray. The result is same as the expectation. However:

```javascript
// fmap (g . f) ≡ fmap g . fmap f -- Associativity
const f = (a) => Promise.resolve(a * 2);
const g = (pa) => pa.then((b) => b * 3);

Promise.resolve(1).then(f).then(g),
// TypeError: pa.then is not a function
```

If `f` return a Promise, the law will be violated. The value that gets passed to f is implicitly unwrapped, causes TypeError

**Answer**: **No**

### Are Promises Monads?

The analogous Promise function for `bind` is `then`. It is same as `fmap`, right? In my option, because JavaScript is dynamic language, to make developers easier to learn, and not confuse about result type, the internal async promises inside `then` is implicitly unwrapped. The analogous function for `return` is `Promise.resolve`.

```javascript
// return a >>= f ≡ f a -- Left identity
const f = (a) => Promise.resolve(a * 2);
const lp = Promise.resolve(1).then(f);
const rp = f(1)

Promise.all([lp, rp]).then(([a, b]) => console.log(a === b));

```

It upholds the law. However, if `a` is a Promise, the law is violated.

```javascript
const f = (m) => m.then((a) => a * 2);

Promise.resolve(Promise.resolve(1)).then(f).then(console.log);
// TypeError: m.then is not a function
```

Because Promise implicitly unwrapped the Promise inside, the value in `then` is no longer Promise. The `TypeError` is raised. Second:

```javascript
// m >>= return ≡ m -- Right identity

Promise.all([
  Promise.resolve(1).then((a) => Promise.resolve(a)),
  Promise.resolve(1)
]).then(([a, b]) => console.log(a === b));
// true
```
Right identity is satisfied, too. Note, we can't use free point style `Promise.resolve(1).then(Promise.resolve)`. `TypeError: PromiseResolve called on non-object` will be raised.

Finally, the Associativity law:

```javascript

//(m >>= f) >>= g ≡	 m >>= (\x -> f x >>= g) -- Associativity
const f = x => Promise.resolve(x * 2);
const g = y => Promise.resolve(y * 3);

Promise.all([
  Promise.resolve(1).then(f).then(g),
  Promise.resolve(1).then((x) => f(x).then(g))
]).then(([a, b]) => console.log(a === b));
```

The law is satisfied. However, it is also violated if the input value is a Promise

```javascript

//(m >>= f) >>= g ≡	 m >>= (\x -> f x >>= g) -- Associativity

const a = Promise.resolve(1);
const f = p => p.then(x => x * 2);
const g = y => Promise.resolve(y * 3);

Promise.all([
  Promise.resolve(a).then(f).then(g),
  Promise.resolve(a).then((x) => f(x).then(g))
]).then(([a, b]) => console.log(a === b));
// TypeError: p.then is not a function
```

**Answer**: **No**

### So, how about Applicative?

Because Applicative class is extended from Functor class, so if Promises don't satisfy Functor class, Applicative don't satisfy too.

```haskell
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

**Answer**: **No**

## Promises in Scala

In Scala, the author divide this asynchronous specification into 2 terms: `Future` and `Promise`, and we usually call it `Future`. In brief:

> A future is a placeholder object for a result that does not yet exist. A promise is a writable, single-assignment container, which completes a future. Promises can complete the future with a result to indicate success, or with an exception to indicate failure.
> [Futures and Promises](https://docs.scala-lang.org/overviews/core/futures.html)

Scala language is a combination of OOP and common function programming with static types. Of course it is much better than JavaScript too. With `map` and `flatMap`, it remove the confusion of JavaScript Promise 

```scala
val fa = Future { 1 }

fa map { a => a * 2 }
// scala.concurrent.Future[Int]
fa map { a => Future { a * 2 } }
// scala.concurrent.Future[scala.concurrent.Future[Int]]

fa flatMap { a => Future { a * 2 } }
// scala.concurrent.Future[Int]

```

**Answer**: **Yes All**

## References

1. [Futures and Promises](http://dist-prog-book.com/chapter/2/futures.html) - Kisalaya Prasad, Avanti Patil, and Heather Miller
2. [Monad Laws](https://wiki.haskell.org/Monad_laws)
3. [Functor](https://wiki.haskell.org/Functor)
4. [Applicative Functors](https://en.wikibooks.org/wiki/Haskell/Applicative_functors)
