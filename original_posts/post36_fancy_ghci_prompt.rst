Giving GHCi a fancy lamda prompt
################################

:date: 2018-01-27
:tags: haskell ghci
:category: Programming
:authors: Tobias Pleyer
:summary: How to set your GHCi REPL prompt to a literal lambda


Write the following code in your GHCi configuration file (~/.ghci)

.. code:: bash

    :set prompt  "\x03BB: "

Which will result in

.. code:: bash

    $ ghci
    GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
    λ: putStrLn "Haskell rocks!"
    Haskell rocks!
    λ:
