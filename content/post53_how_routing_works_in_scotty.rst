How routing works in Scotty
===========================

:date: 2018-08-29
:tags: haskell, functional programming
:category: Programming
:authors: Tobias Pleyer
:summary: A recipe for a light weight, flexible and functional style of protocol decoding


Recap
-----

`In a previous post <{filename}/post52_functional_protocol_decoding.rst>`_ I
introduced a functional approach for decoding protocols.

The main concept was that of so called *interpreters*, of which there can be
defined arbitrarily many, which are tried one after the other until one of them
qualifies to handle a certain input. If no interpreter qualifies a default
interpreter is used.

Here is the key function once again:

.. code:: haskell

    decodeWithInterpreters :: [Interpreter] -> MessageID -> MessageStatus -> InterpreterResult
    decodeWithInterpreters interpreters id status =
      let tryInterpreter (Interpreter curr) next =
            case (curr id status) of
              NotInterpreted -> next
              result -> result
      in foldr tryInterpreter NotInterpreted interpreters

And then I finished the post with a remark about the `Scotty Webframework`_ as
a real world example of this pattern.

.. _Scotty Webframework: https://hackage.haskell.org/package/scotty

This blog post aims to explain how this pattern is used in Scotty to route
requests on a website.

Example
-------

Before we dive into the Scotty code I want to take a moment to give an example
of an application of the simpler *decodeWithInterpreters* function of the above
code sample.

Let's assume we have two interpreters, *int1* and *int2*, an id *id* and a
status *s*. Further we assume *int1* is not able to interpret *id*, so will
return *NotInterpreted* when given *id* and *s*, but *int2* does handle the id
and returns *InterpreterOK "hello example"*. Then we can make the following
reductions.

.. code:: haskell

    decodeWithInterpreters [int1, int2] id s =
    -- function definition
    foldr tryInterpreter NotInterpreted [int1, int2] =
    -- definition of foldr
    int1 `tryInterpreter` (int2 `tryInterpreter` NotInterpreted) =
    -- infix to postfix transformation
    tryInterpreter int1 (tryInterpreter int2 NotInterpreted) =
    -- definition of tryInterpreter
    case (int1 id s) of
      NotInterpreted -> tryInterpreter int2 NotInterpreted
      r -> r
    -- int1 id s = NotInterpreted
    =
    tryInterpreter int2 NotInterpreted
    -- definition of tryInterpreter
    case (int2 id s) of
      NotInterpreted -> NotInterpreted
      r -> r
    =
    InterpreterOK "hello example"

This small code run shows that first *int1* is tried and because it is not able
to interpret the input the next interpreter, *int2* in this case, is tried. I
hope how this code would work if we had more than two Interpreters.

Top level view of a web server
------------------------------

How does serving a website relate to the above code? What does a web server
really need to do deep down the bottom? Well let's assume our website's name is
*example.com*. Then there might be *example.com/home*, *example.com/about* or
*example.com/articles/article42.html*. The content of all these pages will be
different and the server must return the right content for each of them. This
process is called routing. Like in the Linux file system we refer to pages via
a path and the website is the so called root, denoted as *"/"*. All other pages
are given relative to this root. So for our examples we have

    * "/home"
    * "/about"
    * "/articles/article42.html"

Every request to the server will contain one of these so called routing
patterns (possibly with arguments). So an interpreter would generate a HTML
response and the server would send it back to the client.

The we can rewrite the above example like this:

.. code:: haskell

    respond :: [Routes] -> Request -> Response
    respond routes req =
      let tryRoute thisRoute nextRoute =
            case thisRoute req of
              resp@(Response _) -> resp
              NoResponse -> nextRoute
      in foldr tryRoute errorResponse routes

This is a very primitive web server implementation. The *errorResponse* is the
default handler, usually means an error and could generate a `404`_.

.. _404: https://en.wikipedia.org/wiki/HTTP_404

Ok, enough of the "Hello world!" stuff, let's beam over to the real world!

    Scotty, beam me up!

Scotty
------

This post is not meant as an introduction to Scotty, so I will glance over many
types and aspects. However it is good to know that Scotty uses the `Warp`_
server under the hoods and conforms to the `Web Application Interface`_ (WAI). As a
consequence it also uses the terms *Middleware* and *Application*:

.. _Warp: http://hackage.haskell.org/package/warp
.. _Web Application Interface: https://hackage.haskell.org/package/wai

.. code:: haskell

    type Middleware m = Application m -> Application m
    type Application m = Request -> m Response

**Note:** The Scotty code in this section is taken from `Scotty's github`_.

.. _Scotty's github: https://github.com/scotty-web/scotty/tree/0.11.2

An application is a function which knows how to return a request as a response
in a monadic context (the **m** is a `Monad`_). A middleware is meant to
interfere with the request/response process. The middleware intercepts the
request, does something with it and all follow up code uses the modified value.
A possible use case would be a request containing gzipped content. A middleware
could be installed to unzip the content. The actual application code can stay
more generic, because it doesn't have to deal with the details of the content
type. As a result a middleware is a function which takes an existing
application and returns a new application, with the middleware's functionality
baked into it.

.. _Monad: https://wiki.haskell.org/Monad

The following code shows the main Scotty application, which does the routing,
and the functions responsible for adding routes to the definition of the
application:

.. code-block:: haskell
    :linenos: inline

    data ScottyState e m =
        ScottyState { middlewares :: [Wai.Middleware]
                    , routes :: [Middleware m]
                    , handler :: ErrorHandler e m
                    }

    scottyAppT :: (Monad m, Monad n)
               => (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
               -> ScottyT e m ()
               -> n Application
    scottyAppT runActionToIO defs = do
        let s = execState (runS defs) def
        let rapp req callback = runActionToIO (foldl (flip ($)) notFoundApp (routes s) req) >>= callback
        return $ foldl (flip ($)) rapp (middlewares s)

    notFoundApp :: Monad m => Scotty.Application m
    notFoundApp _ = return $ responseBuilder status404 [("Content-Type","text/html")]
                           $ fromByteString "<h1>404: File Not Found!</h1>"

    addRoute :: Middleware m -> ScottyState e m -> ScottyState e m
    addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

    addroute :: (ScottyError e, MonadIO m) => StdMethod
                                           -> RoutePattern
                                           -> ActionT e m ()
                                           -> ScottyT e m ()
    addroute method pat action =
      ScottyT $ MS.modify $ \s -> addRoute (route (handler s) (Just method) pat action) s

    route :: (ScottyError e, MonadIO m) => ErrorHandler e m
                                        -> Maybe StdMethod
                                        -> RoutePattern
                                        -> ActionT e m ()
                                        -> Middleware m
    route h method pat action app req =
        let tryNext = app req
            methodMatches :: Bool
            methodMatches =
                case method of
                    Nothing -> True
                    Just m -> Right m == parseMethod (requestMethod req)
        in if methodMatches
           then case matchRoute pat req of
                Just captures -> do
                    env <- mkEnv req captures
                    res <- runAction h env action
                    maybe tryNext return res
                Nothing -> tryNext
           else tryNext

Ok let's try to connect the dots. The **notFoundApp** application will the
default behaviour if the requested site is unknown. As we can see it generates
the well known *404* response.

The **scottyAppT** will be fed to the *Warp* web server and constitutes the
entry point to the application logic in the WAI architecture. Let's look at its
arguments. The **runActionsToIO** function can be ignored for our purposes. Its
purpose is to do "the dirty work" in the IO Monad, i.e. the actual interaction
with the outside world. The **defs** argument contains the defintion of the
server, i.e. the raw information that makes up the server. We can see that the
first thing that is done in the do block is to actually evaluate the definition
of the server (line 12). The result will be of type *ScottyState*. In the
process of evaluating the definition the routes will be added. This is done via
the **addRoute** function. The **addRoute** function makes use of a helper
function **route** which serves as constructor for Middleware type. At this
point it is maybe not easy to see yet, but **route** serves a very similar role
as the *tryRoute* function in the entry example. Once the route middleware was
created (constructed) it is simple prepended to a list within the
*ScottyState*. This list of routes corresponds to the *routes* list of our
entry example. In line 13 we can see how this list of routes is extracted and
used with a left fold. This left fold performs the "routing interpreter lookup"
as mentioned earlier in this post.
