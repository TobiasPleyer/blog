---
title:  "Haskell's prelude list index function"
date: 2018-02-27
tags: haskell
category: Programming
authors: Tobias Pleyer
summary: "A case study of the awesome techniques of functional programming"
---

Haskell's prelude list index function
=====================================

Indexing lists in Haskell
-------------------------

Haskell's [Prelude](hackage.haskell.org/package/base/docs/Prelude.html)
and the
[Data.List](hackage.haskell.org/package/base/docs/Data-List.html) module
both export the [!!]{.title-ref} operator to take the index of a list.

Haskell's standard list type is defined as follows

``` {.sourceCode .haskell}
data List a = Nil | Cons a (List a)
```

Which means that a list is either empty (*Nil*) or consists of an
element and a reference (link) to the rest of the list, which can be
empty or have more elements. Such an implementation is called [singly
linked
lists](https://en.wikipedia.org/wiki/Linked_list#Singly_linked_list).

Singly linked list do not provide a direct index lookup, because they do
not hold information how to extract the element directly. Instead, if
one wants to to have the element at index [i]{.title-ref}, he has to
walk the linked list and follow the links [i]{.title-ref} times.

One possible implementation would be

``` {.sourceCode .haskell}
myindex :: [a] -> Int -> a
myindex [] _ = error "Index too large!"
myindex (x:xs) i = case i of
    0 -> x
    _ -> myindex xs (i-1)
```

The implementation is very straight forward: We pattern match the list.
If we reached the empty list we have no chance to find an element
anymore and we error out. Note that this will throw an exception at run
time and is thus unsafe. This is equivalent to the implementation of
Haskell's [!!]{.title-ref} operator. Otherwise we pattern match the
current element and the rest of the list, if our index is 0, we know
what to do, the 0th index of a list is the first element, so we return
it. But if the index is bigger than zero then we decrement it and
recurse with the rest of the list.

**Note**: I left out a check for negative indices.

The Data.List implementation
----------------------------

Now we come to the reason for this post: The actual implementation of
[!!]{.title-ref} in Data.List. I think it is a very nice and simple
example of the fascinating options functional and lazy programming
provide.

First the implementation:

``` {.sourceCode .haskell}
tooLarge :: Int -> a
tooLarge _ = errorWithoutStackTrace (prel_list_str ++ "!!: index too large")

negIndex :: a
negIndex = errorWithoutStackTrace $ prel_list_str ++ "!!: negative index"

xs !! n
  | n < 0     = negIndex
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) tooLarge xs n
```

**Note:** Actually there are two implementations of [!!]{.title-ref} in
the package, guarded by macros to choose only one of them. The version
above is the second option which avoids inlining calls to error. The
first option is actually the implementation I gave in the introduction.

Let's have a closer look at how this piece of code works. First the
implementation of foldr:

``` {.sourceCode .haskell}
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x:xs) = f x (foldr f b xs)
```

So far so good. Often [f]{.title-ref} used in [foldr]{.title-ref} is a
binary function which produces a *'primitive'* result, like an integer.
For example in the case of one of the most used folding examples:

``` {.sourceCode .haskell}
let sum = foldr (+) 0
sum [1..5]
15
```

**Disclaimer**: Memory usage and strictness considerations are not of
interest here. The given implementation of sum is one of many possible
implementations.

But the return value of the folding function is not limited to these
cases. Functions for example are also first class values in Haskell and
totally valid as return value of the folding function.

Because Haskell features
[currying](https://en.wikipedia.org/wiki/Currying) the effect might be a
bit obfuscated in the above example, but we can make the implicit
function return value explicit:

``` {.sourceCode .haskell}
f :: Int -> a
f x r = \k -> case k of
                0 -> x
                _ -> r (k-1)

xs !! n
  | n < 0     = negIndex
  | otherwise = foldr f tooLarge xs n
```

The more interesting point is that the returned function is a
[closure](https://en.wikipedia.org/wiki/Closure_(computer_programming))
(lexically scoped name binding) of the other two parameters, namely the
current element of the list ([x]{.title-ref}) and the remaining fold of
functions ([r]{.title-ref}). The variable [k]{.title-ref} is later bound
to the requested index.

So what this function does is create a function which takes one
parameter, the index, and either returns the bound value of the list
element if the index is zero, or calls the continuation function
(created by foldr) with the decremented index. This is equivalent to the
sample implementation given at the beginning.

Let's see the implementation in action:

``` {.sourceCode .haskell}
let list = ['a'..'z']
let f x r = \k -> case k of
              0 -> x
              _ -> r (k-1)
list !! 3
=
foldr f tooLarge list 3
=
f 'a' (foldr f tooLarge ['b'..'z']) 3
=
case 3 of
  0 -> 'a'
  _ -> (foldr f tooLarge ['b'..'z']) (3-1)
=
foldr f tooLarge ['b'..'z'] 2
=
f 'b' (foldr f tooLarge ['c'..'z']) 2
=
case 2 of
  0 -> 'b'
  _ -> (foldr f tooLarge ['c'..'z']) (2-1)
=
foldr f tooLarge ['c'..'z'] 1
=
f 'c' (foldr f tooLarge ['d'..'z']) 1
=
case 1 of
  0 -> 'c'
  _ -> (foldr f tooLarge ['d'..'z']) (1-1)
=
foldr f tooLarge ['d'..'z'] 0
=
f 'd' (foldr f tooLarge ['e'..'z']) 0
=
case 0 of
  0 -> 'd'
  _ -> (foldr f tooLarge ['e'..'z']) (0-1)
= 'd'
```

Note that due to lazy evaluation we just calculate as much as necessary
to yield a result and do not have to fear the creation of the additional
helper closures.