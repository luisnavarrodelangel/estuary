
Now we experiment with building widgets for more complex types built on top of
the Simple type, and using the simpleWidget definition. For example, a tuplet
composed of a pair of Simple values and edited with a pair of simpleWidgets.

First create a new module for the Simple Type and simpleWidget function. Save the code in an independent file as Simple.hs

> {-# LANGUAGE RecursiveDo #-}
> module Simple where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2

> data Simple = One | Two | Three deriving (Show)

> simpleWidget :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> simpleWidget i = el "div" $ do
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn i (leftmost buttons)
>   display value
>   return value

Create a new file and type the following code.

> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2
> import Simple


> type Tuplet = (Simple,Simple)
>
> tupleWidget :: MonadWidget t m => Tuplet -> m (Dynamic t Tuplet)
> tupleWidget (i,j) = el "div" $ do
>   i' <- simpleWidget i
>   j' <- simpleWidget j
>   combineDyn (\x y -> "composite:" ++ show x ++ " " ++ (show y)) i' j' >>= display
>   combineDyn (,) i' j'
>
>   main = mainWidget $ tupleWidget (One, One) >>= display
