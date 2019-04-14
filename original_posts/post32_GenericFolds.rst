Generic Folds in Haskell
########################

:date: 2018-01-07
:tags: haskell
:category: Programming
:authors: Tobias Pleyer
:summary: A blog post about fix-point data types and generic folding


Introduction
============

In December I found the `Advent of Code`_ tasks to be a nice opportunity to
test and improve my skills in Haskell. I published my solutions on `my
github`_.

I was also interested in the solutions of other people and found `Eric Mertens
github`_ in the ranking list. Eric's solutions display a deep understanding of
Haskell and all its beauty. The solution of day 7 was especially astonishing
for me. That's where I first saw the concept of generic folding in the form of
anamorphisms and catamorphisms. In my own first attempt I tried to do exactly
what Eric did in his solution, but I didn't have the toolset to do so and
ended up going a simpler route.

I began to interest myself for the topic and started to investigate it a bit
further. There exists the package `Data.Fix`_ on hackage and much of the code
in this article is taken from there. Coincidentally I read Mark P. Jones' (MPJ)
paper **Functional Programming with Overloading and Higher-Order Polymorphism**
a few days after I saw Erics' solution for day 7 and to my surprise discovered
that this paper also devotes a full sub-section to this topic. A few examples
of this blog post are taken from this paper.

.. _Advent of Code: http://adventofcode.com/
.. _my github: https://github.com/TobiasPleyer/aoc
.. _Eric Mertens github: https://github.com/glguy/advent2017
.. _Data.Fix: https://hackage.haskell.org/package/data-fix/docs/Data-Fix.html

Fix-point data types
====================

The first step to understanding generic folding involves fix points of data
types. What is a fix point? The usual definition applies to plain functions and
describes a value that remains unchanged by function application, i.e. is its
own return value

.. code-block:: haskell

    let x = Fix(f)
    f x = x

Since a constructor is basically a function on types, it is possible to define
fix points on types as well

.. code-block:: haskell

    newtype Fix f = Fix { unFix :: f (Fix f) }

Note that f in the above snippet represents a type, not a plain value, and
should be an instance of *Functor* or *Traversable* (we will see why later). A
fix point data type is a type that is applied to itself, possibly indefinitly.

This concept is a bit strange and it is best to see it with some examples

.. code-block:: haskell

    type List a = Fix (L a)
    data L a b = Nil | Cons a b
    let list_example :: List Int
        list_example = Fix (Cons 4 (Fix (Cons 3 (Fix (Cons 2 (Fix Cons 1 (Fix Cons 0 (Fix Nil))))))))

    type Natural = Fix Nat
    data Nat = Zero | Succ s
    let natural_3 :: Natural
        natural_3 = Fix (Succ (Fix (Succ (Fix (Succ (Fix Zero))))))

    -- This is the example of Eric Mertens
    type RoseTree = Fix Node
    data Node a = Node !Int [a]
    let rosetree_example :: RoseTree
        rosetree_example = Fix (Node 1 Fix ([Node 2 Fix ([Node 3 Fix ([]), Node 4 Fix ([])]), Node 5 Fix ([])]))

Ana-, Cata-, and Hylomorphism
=============================

Now that we have seen a few examples of fix-point data types, what are their
advantage? How can we work with them? Let's quote the *Data.Fix* package

    *This style allows you to express recursive functions in non-recursive
    manner.*

Ok let's elaborate on that. A normal recursive function has parameters,
performs some actions with them and then calls itself with altered parameters
infinitely until the base case is hit. A classic example is the factorial
function

.. code-block:: haskell

    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial n = n * factorial (n-1)

In the above example the integer parameter *n* is very simple and the final
result of the function call is made up of many other calls to the same function
with n decremented. With fix-point types we can kind of reverse that concept
and take a normal non-recursive function and recurse its application into the
data type. The function that does that is called a `catamorphism`_. Here is its
definition

.. _catamorphism: https://en.wikipedia.org/wiki/Catamorphism

.. code-block:: haskell

    cata :: Functor f => (f a -> a) -> (Fix f -> a)
    cata h = h . fmap (cata h) . unFix

The definition of `cata` shows why we had to require `f` to be a `Functor`,
because we use `fmap` to descent into the data structure. `Cata` is also called
a generic function fold. Note how `cata` works: Before we apply the function
`h` we recurse further into the fix-point type by `fmapping` into it. This
means the first call to `h` is applied last. We will see examples soon.

Intermezzo - standard folds
---------------------------

Above I always talk about **generic folds**, but what are **"normal folds"**
then? A normal folding function takes a list of values and a function to
combine these values and then iterates over the list of elements. It starts
applying the function to the first element, remembers the result and then
combines that result with the function application to the next element of the
list. This repeats until the empty list is found, that's the base case that
causes the function to stop. There exists not one, but many folding functions
in Haskell Prelude or Data.List, e.g. `foldr`, `foldl`, `foldr1`, ...

.. code-block:: haskell

    import Data.List
    -- calculate the sum of the elements in a list - version 1
    let s = foldr (+) 0 [1,2,3,4]
    -- calculate the sum of the elements in a list - version 2
    let s2 = foldr1 (+) [1,2,3,4]
    -- make one big list out of a list of lists - version 1
    let big = foldl' (++) [] [[1,2,3], [4,5,6], [7,8,9]]
    -- make one big list out of a list of lists - version 2
    let big2 = foldl1' (++) [[1,2,3], [4,5,6], [7,8,9]]

Generic folds continued
-----------------------

We can replicate the above folds on lists using our generic framework. The
first thing we have to do is represent our lists as a fix-point, but we have
already done this above

.. code-block:: haskell

    #!/usr/bin/env stack
    {- stack
      script
      --resolver lts-9.12
      --package base
      --package data-fix
    -}

    import Data.Fix

    data L a b = Nil | Cons a b

    type List a = Fix (L a)

    instance Functor (L a) where
       fmap f x = case x of
           Nil      -> Nil
           Cons a b -> Cons a (f b)

    fix_length :: List a -> Int
    fix_length = cata $ \x -> case x of
       Nil      -> 0
       Cons _ n -> n + 1

    -- this is the equivalent to foldr1 (+)
    fix_sum :: Num a => List a -> a
    fix_sum = cata $ \x -> case x of
       Nil      -> 0
       Cons a s -> a + s

    -- this is the equivalent to foldr (+) b
    fix_sum_base :: Num a => a -> List a -> a
    fix_sum_base b xs = res b
      where
        res = c xs
        c = cata $ \x -> case x of
               Nil      -> (+ 0)
               Cons a s -> (+ (s a))

    main = do
      -- the list [1,2,3,4] = 1 : 2 : 3 : 4 : []
      let list = Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix (Cons 4 (Fix Nil))))))))
      let s = fix_sum list
      print s
    -- 10
      let s2 = fix_sum_base 5 list
      print s2
    -- 15

Note in the example above that `fix_sum` is the equivalent to `foldr1 (+)`,
because there is no base case as in `foldr`. As we would expect it is possible
to derive the special case of folding on lists from the generic case.

The dual of a catamorphism is called anamorphism, or generic unfold. It does
exactly the opposite of a catamorphism, i.e. constructing a fix-point type with
the help of a provided function.

.. code-block:: haskell

    ana :: Functor f => (a -> f a) -> (a -> Fix f)
    ana f = Fix . fmap (ana f) . f

Here is the example of an anamorphism generating the infinite list of positive
integers

.. code-block:: haskell

    let posInts = (ana (\n -> Cons n (n+1))) 1

The last of the group is the hylomorphism, which is an anamorphism followed by
a catamorphism. Thus a hylomorphism first constructs (unfolds) a fix-point data
type and then deconstructs (folds) it again.

.. code-block:: haskell

    -- example of a hylomorphism that calculates the sum of a range on integers
    sum_from_to
      :: Int -- ^ start of the integer range
      -> Int -- ^ end of the integer range
      -> Int -- ^ sum of the integers [start..end]
    sum_from_to start end = (hylo phi (psi end)) start
      where
        psi m n
          | n > m = Nil
          | otherwise = Cons n (n+1)
        phi x = case x of
           Nil      -> 0
           Cons a s -> a + s

Handson - Taking apart the AOC Day 7 solution
=============================================

In this section I will try to give a detailed look of Eric Merten's solution.
Since the original input for day 7 is to much to type it by hand, I will
content myself with the example input. Thusly we start with the following table

.. code-block:: python

    pbga (66)
    xhth (57)
    ebii (61)
    havc (66)
    ktlj (57)
    fwft (72) -> ktlj, cntj, xhth
    qoyq (66)
    padx (45) -> pbga, havc, qoyq
    tknk (41) -> ugml, padx, fwft
    jptl (61)
    ugml (68) -> gyxo, ebii, jptl
    gyxo (61)
    cntj (57)

Which will result in the following tree structure

.. code-block:: python

    # Resulting tree
                    gyxo
                  /
             ugml - ebii
           /      \
          |         jptl
          |
          |         pbga
         /        /
    tknk --- padx - havc
         \        \
          |         qoyq
          |
          |         ktlj
           \      /
             fwft - cntj
                  \
                    xhth

Next we need a suitable data type to represent the tree. Every node has a name
(for lookup purposes), a weight and zero to more children. As I learned in the
paper of MPJ these kind of trees are called `rose trees`_.

.. _rose trees: https://en.wikipedia.org/wiki/Rose_tree

.. code-block:: haskell

    data Node a = Node !Int [a]
        deriving (Show, Functor, Foldable, Traversable)

Note the automagic deriving of *Show*, *Functor*, *Foldable* and *Traversable*.
This will lead to the following implementation of e.g. `fmap`

.. code-block:: haskell

    instance Functor Node
      where
        fmap f (Node n xs) = Node n (map f xs)

Now we can construct our tree

.. code-block:: haskell

    import Data.Map as Map

    let input :: Map String (Node String)
        input = Map.fromList
          [ ("pbga", Node 66 [])
          , ("xhth", Node 57 [])
          , ("ebii", Node 61 [])
          , ("havc", Node 66 [])
          , ("ktlj", Node 57 [])
          , ("fwft", Node 72 ["ktlj","cntj","xhth"])
          , ("qoyq", Node 66 [])
          , ("padx", Node 45 ["pbga","havc","qoyq"])
          , ("tknk", Node 41 ["ugml","padx","fwft"])
          , ("jptl", Node 61 [])
          , ("ugml", Node 68 ["gyxo","ebii","jptl"])
          , ("gyxo", Node 61 [])
          , ("cntj", Node 57 [])]

    let root_node = "tknk"

    let tree = (ana (input Map.!)) root_node

The last line is the most interesting, so let's see the steps taken to
construct the tree.

**Note:** `Map.!` is the lookup function maps, e.g.

.. code-block:: haskell

    input Map.! "havc" = Node 66 []

So...

.. code-block:: haskell

    tree =
    (ana (input Map.!)) root_node =
    -- apply definition of ana
    (Fix . fmap (ana (input Map.!)) . (input Map.!)) "tknk" =
    -- perform a Map lookup
    (Fix . fmap (ana (input Map.!))) (Node 41 ["ugml","padx","fwft"]) =
    -- apply fmap
    Fix (Node 41 (map (ana (input Map.!)) ["ugml","padx","fwft"])) =
    -- apply map
    Fix (Node 41 [(ana (input Map.!)) "ugml"
                 ,(ana (input Map.!)) "padx"
                 ,(ana (input Map.!)) "fwft"]) =
    -- apply definition of ana
    Fix (Node 41 [(Fix . fmap (ana (input Map.!)) . (input Map.!)) "ugml"
                 ,(Fix . fmap (ana (input Map.!)) . (input Map.!)) "padx"
                 ,(Fix . fmap (ana (input Map.!)) . (input Map.!)) "fwft"]) =
    -- perform a Map lookup
    Fix (Node 41 [(Fix . fmap (ana (input Map.!))) (Node 68 ["gyxo","ebii","jptl"])
                 ,(Fix . fmap (ana (input Map.!))) (Node 45 ["pbga","havc","qoyq"])
                 ,(Fix . fmap (ana (input Map.!))) (Node 72 ["ktlj","cntj","xhth"])]) =
    -- apply fmap
    Fix (Node 41 [Fix (Node 68 (map (ana (input Map.!)) ["gyxo","ebii","jptl"]))
                 ,Fix (Node 45 (map (ana (input Map.!)) ["pbga","havc","qoyq"]))
                 ,Fix (Node 72 (map (ana (input Map.!)) ["ktlj","cntj","xhth"]))]) =
    -- apply map
    Fix (Node 41 [Fix (Node 68 [(ana (input Map.!)) "gyxo"
                               ,(ana (input Map.!)) "ebii"
                               ,(ana (input Map.!)) "jptl"])
                 ,Fix (Node 45 [(ana (input Map.!)) "pbga"
                               ,(ana (input Map.!)) "havc"
                               ,(ana (input Map.!)) "qoyq"])
                 ,Fix (Node 72 [(ana (input Map.!)) "ktlj"
                               ,(ana (input Map.!)) "cntj"
                               ,(ana (input Map.!)) "xhth"])]) =
    -- apply definition of ana
    Fix (Node 41 [Fix (Node 68 [(Fix . fmap (ana (input Map.!)) . (input Map.!)) "gyxo"
                               ,(Fix . fmap (ana (input Map.!)) . (input Map.!)) "ebii"
                               ,(Fix . fmap (ana (input Map.!)) . (input Map.!)) "jptl"])
                 ,Fix (Node 45 [(Fix . fmap (ana (input Map.!)) . (input Map.!)) "pbga"
                               ,(Fix . fmap (ana (input Map.!)) . (input Map.!)) "havc"
                               ,(Fix . fmap (ana (input Map.!)) . (input Map.!)) "qoyq"])
                 ,Fix (Node 72 [(Fix . fmap (ana (input Map.!)) . (input Map.!)) "ktlj"
                               ,(Fix . fmap (ana (input Map.!)) . (input Map.!)) "cntj"
                               ,(Fix . fmap (ana (input Map.!)) . (input Map.!)) "xhth"])]) =
    -- perform a Map lookup
    Fix (Node 41 [Fix (Node 68 [(Fix . fmap (ana (input Map.!))) (Node 61 [])
                               ,(Fix . fmap (ana (input Map.!))) (Node 61 [])
                               ,(Fix . fmap (ana (input Map.!))) (Node 61 [])])
                 ,Fix (Node 45 [(Fix . fmap (ana (input Map.!))) (Node 66 [])
                               ,(Fix . fmap (ana (input Map.!))) (Node 66 [])
                               ,(Fix . fmap (ana (input Map.!))) (Node 66 [])])
                 ,Fix (Node 72 [(Fix . fmap (ana (input Map.!))) (Node 57 [])
                               ,(Fix . fmap (ana (input Map.!))) (Node 57 [])
                               ,(Fix . fmap (ana (input Map.!))) (Node 57 [])])]) =
    -- apply fmap
    Fix (Node 41 [Fix (Node 68 [Fix (Node 61 (map (ana (input Map.!)) []))
                               ,Fix (Node 61 (map (ana (input Map.!)) []))
                               ,Fix (Node 61 (map (ana (input Map.!)) []))])
                 ,Fix (Node 45 [Fix (Node 66 (map (ana (input Map.!)) []))
                               ,Fix (Node 66 (map (ana (input Map.!)) []))
                               ,Fix (Node 66 (map (ana (input Map.!)) []))])
                 ,Fix (Node 72 [Fix (Node 57 (map (ana (input Map.!)) []))
                               ,Fix (Node 57 (map (ana (input Map.!)) []))
                               ,Fix (Node 57 (map (ana (input Map.!)) []))])]) =
    -- apply map
    Fix (Node 41 [Fix (Node 68 [Fix (Node 61 [])
                               ,Fix (Node 61 [])
                               ,Fix (Node 61 [])])
                 ,Fix (Node 45 [Fix (Node 66 [])
                               ,Fix (Node 66 [])
                               ,Fix (Node 66 [])])
                 ,Fix (Node 72 [Fix (Node 57 [])
                               ,Fix (Node 57 [])
                               ,Fix (Node 57 [])])]) =

As one can see this is the same tree as drawn above. Also note how the empty
list serves as base case which prevents `fmap (ana (input Map.!))` to recurse
forever.

Now we can have a look at the catamorphism. I use a simpler *phi* function (the
argument of `cata`) than in the original solution of Eric Mertens, because it
would be really tedious to write all the intermediate steps down in this blog
post. However the solution is identical in spirit.

.. code-block:: haskell

    data Summary = Summary
        { weight ::!Int         -- ^ total node weight
        , children :: [Summary] -- ^ the children summaries
        } deriving Show

    phi :: Node Summary -> Summary
    phi (Node n xs) =
      if (null xs)
      then
        Summary n []
      else
        Summary (n + (sum . (map weight)) xs) xs

    summarize = cata phi

With these definitions at hand let's `run` the code

.. code-block:: haskell

    summary =
    summarize tree =
    -- value of tree
    summarize (Fix (Node 41 [Fix (Node 68 [Fix (Node 61 [])
                                          ,Fix (Node 61 [])
                                          ,Fix (Node 61 [])])
                            ,Fix (Node 45 [Fix (Node 66 [])
                                          ,Fix (Node 66 [])
                                          ,Fix (Node 66 [])])
                            ,Fix (Node 72 [Fix (Node 57 [])
                                          ,Fix (Node 57 [])
                                          ,Fix (Node 57 [])])])) =
    -- definition of summarize
    (cata phi) (Fix (Node 41 [Fix (Node 68 [Fix (Node 61 [])
                                           ,Fix (Node 61 [])
                                           ,Fix (Node 61 [])])
                             ,Fix (Node 45 [Fix (Node 66 [])
                                           ,Fix (Node 66 [])
                                           ,Fix (Node 66 [])])
                             ,Fix (Node 72 [Fix (Node 57 [])
                                           ,Fix (Node 57 [])
                                           ,Fix (Node 57 [])])])) =
    -- definition of cata
    (phi . fmap (cata phi) . unFix) (Fix (Node 41 [Fix (Node 68 [Fix (Node 61 [])
                                                                ,Fix (Node 61 [])
                                                                ,Fix (Node 61 [])])
                                                  ,Fix (Node 45 [Fix (Node 66 [])
                                                                ,Fix (Node 66 [])
                                                                ,Fix (Node 66 [])])
                                                  ,Fix (Node 72 [Fix (Node 57 [])
                                                                ,Fix (Node 57 [])
                                                                ,Fix (Node 57 [])])])) =
    -- function application
    (phi . fmap (cata phi)) (Node 41 [Fix (Node 68 [Fix (Node 61 [])
                                                   ,Fix (Node 61 [])
                                                   ,Fix (Node 61 [])])
                                     ,Fix (Node 45 [Fix (Node 66 [])
                                                   ,Fix (Node 66 [])
                                                   ,Fix (Node 66 [])])
                                     ,Fix (Node 72 [Fix (Node 57 [])
                                                   ,Fix (Node 57 [])
                                                   ,Fix (Node 57 [])])]) =
    -- definition of fmap
    phi (Node 41 (map (cata phi) [Fix (Node 68 [Fix (Node 61 [])
                                               ,Fix (Node 61 [])
                                               ,Fix (Node 61 [])])
                                 ,Fix (Node 45 [Fix (Node 66 [])
                                               ,Fix (Node 66 [])
                                               ,Fix (Node 66 [])])
                                 ,Fix (Node 72 [Fix (Node 57 [])
                                               ,Fix (Node 57 [])
                                               ,Fix (Node 57 [])])])) =
    -- definition of fmap
    phi (Node 41 [(cata phi) (Fix (Node 68 [Fix (Node 61 [])
                                           ,Fix (Node 61 [])
                                           ,Fix (Node 61 [])]))
                 ,(cata phi) (Fix (Node 45 [Fix (Node 66 [])
                                           ,Fix (Node 66 [])
                                           ,Fix (Node 66 [])]))
                 ,(cata phi) (Fix (Node 72 [Fix (Node 57 [])
                                           ,Fix (Node 57 [])
                                           ,Fix (Node 57 [])]))])) =
    -- definition of cata + unwrap Fix + apply fmap + map
    phi (Node 41 [phi (Node 68 [(cata phi) Fix (Node 61 [])
                               ,(cata phi) Fix (Node 61 [])
                               ,(cata phi) Fix (Node 61 [])])
                 ,phi (Node 45 [(cata phi) Fix (Node 66 [])
                               ,(cata phi) Fix (Node 66 [])
                               ,(cata phi) Fix (Node 66 [])])
                 ,phi (Node 72 [(cata phi) Fix (Node 57 [])
                               ,(cata phi) Fix (Node 57 [])
                               ,(cata phi) Fix (Node 57 [])])]) =
    -- definition of cata + unwrap Fix + apply fmap + map
    phi (Node 41 [phi (Node 68 [phi (Node 61 [])
                               ,phi (Node 61 [])
                               ,phi (Node 61 [])])
                 ,phi (Node 45 [phi (Node 66 [])
                               ,phi (Node 66 [])
                               ,phi (Node 66 [])])
                 ,phi (Node 72 [phi (Node 57 [])
                               ,phi (Node 57 [])
                               ,phi (Node 57 [])])]) =
    -- definition of phi
    phi (Node 41 [phi (Node 68 [ Summary 61 []
                               , Summary 61 []
                               , Summary 61 []])
                 ,phi (Node 45 [ Summary 66 []
                               , Summary 66 []
                               , Summary 66 []])
                 ,phi (Node 72 [ Summary 57 []
                               , Summary 57 []
                               , Summary 57 []])]) =
    -- definition of phi
    phi (Node 41 [Summary (68 + (sum . (map weight)) [ Summary 61 []
                                                     , Summary 61 []
                                                     , Summary 61 []]) ([ Summary 61 []
                                                                        , Summary 61 []
                                                                        , Summary 61 []])
                 ,Summary (45 + (sum . (map weight)) [ Summary 66 []
                                                     , Summary 66 []
                                                     , Summary 66 []]) ([ Summary 66 []
                                                                        , Summary 66 []
                                                                        , Summary 66 []])
                 ,Summary (72 + (sum . (map weight)) [ Summary 57 []
                                                     , Summary 57 []
                                                     , Summary 57 []]) ([ Summary 57 []
                                                                        , Summary 57 []
                                                                        , Summary 57 []])]) =
    -- apply map and sum
    phi (Node 41 [Summary (68 + 183) [ Summary 61 []
                                     , Summary 61 []
                                     , Summary 61 []]
                 ,Summary (45 + 198) [ Summary 66 []
                                     , Summary 66 []
                                     , Summary 66 []]
                 ,Summary (72 + 171) [ Summary 57 []
                                     , Summary 57 []
                                     , Summary 57 []]]) =
    -- apply addition
    phi (Node 41 [Summary 251 [ Summary 61 []
                              , Summary 61 []
                              , Summary 61 []]
                 ,Summary 243 [ Summary 66 []
                              , Summary 66 []
                              , Summary 66 []]
                 ,Summary 243 [ Summary 57 []
                              , Summary 57 []
                              , Summary 57 []]]) =
    -- definition of phi + apply map and sum
    Summary 778 [Summary 251 [ Summary 61 []
                             , Summary 61 []
                             , Summary 61 []]
                ,Summary 243 [ Summary 66 []
                             , Summary 66 []
                             , Summary 66 []]
                ,Summary 243 [ Summary 57 []
                             , Summary 57 []
                             , Summary 57 []]]

The complete script is `here <{filename}/code/GenericFolds.hs>`_. Note that the
above `Summary` data type is not the final solution to day 7, but it nicely
shows how we use generic folding to tackle problems with trees involved.
