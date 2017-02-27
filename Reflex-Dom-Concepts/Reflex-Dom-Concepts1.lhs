> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2

In these examples, we'll build Reflex.Dom widgets around a simple pure data
type, and (soon) more complex types built up out of that simple pure type.
Here's the type, which has exactly three possible values (data constructors).
We are automatically deriving definitions for the Show type class, which will
undoubtedly be useful at various points.

> data Simple = One | Two | Three deriving (Show)

Our widgets will have a return type of m (Dynamic t a). In this first set of
examples, we'll focus on widgets that are meant to manipulate just the simple
type, so their return type will be m (Dynamic t Simple).

As a first attempt, we'll make a widget that has three buttons and whichever
button is pressed most recently defines the value returned by the widget as a
whole.

> simpleWidget' :: MonadWidget t m => m (Dynamic t Simple)
> simpleWidget' = do
>  a <- button "One"
>  b <- button "Two"
>  c <- button "Three"
>  let d = fmap (\() -> One) a
>  let e = fmap (\() -> Two) b
>  let f = fmap (\() -> Three) c
>  holdDyn One $ leftmost [d,e,f]

> main = mainWidget $ simpleWidget' >>= display

In the code above, we have buttons that produce Events containing an empty (),
signaling that something has happened but nothing beyond that. We map those
into Events that contain values corresponding to the meaning of which button
was pressed. We use leftmost to turn those three streams of Events into a
single Event stream. Finally holdDyn lets us turn this into a Dynamic value,
with a default value (One) chosen from our type as the initial value of this
Dynamic.

In the IO () program defined by main, we bing simpleWidget to 'display', taking
advantage of the fact that our Simple type is a member of the class Show. (To
verify that the examples below work you'll need to chanfe the definition of main

It's a bit tedious the way we have three lines of code to make buttons and then
another three lines of code just, effectively, to make the buttons trigger a
different, specific event (One, Two or Three instead of ()). This next example
shows three similar and more econimical ways of mapping what is returned by the
buttons:

> simpleWidget'' :: MonadWidget t m => m (Dynamic t Simple)
> simpleWidget'' = do
>   a <- liftM (fmap (\_ -> One)) $ button "One" -- :: m (Event t Simple)
>   b <- liftM (fmap (const Two)) $ button "Two"
>   c <- liftM (Three <$) $ button "Three"
>   holdDyn One $ leftmost [a,b,c] -- m (Dynamic t Simple)
> main = mainWidget $ simpleWidget'' >>= display

Now, for an exercise, let's eliminate the rundandancy of three lines for three
buttons, when we can just make a list of the elements to be "buttonized", taking
advantage of the fact that our simple type derives Show to "automatically"
generate labels for the buttons.

> simpleWidget''' :: MonadWidget t m => m (Dynamic t Simple)
> simpleWidget''' = do
>   a <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   holdDyn One $ leftmost a
> main =  mainWidget $ simpleWidget''' >>= display

In a final, definitive variation on our basic widget for our Simple type, we add
the ability to specify the initial Simple value of the widget, and wrap
everything in a <div> element so that each widget will appear on a new "line" in
the browser. (In a further variation we could add CSS styles to both the buttons
and the containing div).

> simpleWidget :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> simpleWidget i = el "div" $ do
>   buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   holdDyn i (leftmost buttons)
> main = mainWidget $ (simpleWidget One)  >>= display

Can we do the above using the applicative style instead?

> simpleWidgetA :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> simpleWidgetA i = el "div" $ do
>   a <- button "One"
>   b <- button "Two"
>   c <- button "Three"
>   let d = const One <$> a
>   let e = const Two <$> b
>   let f = const Three <$> c
>   holdDyn i $ leftmost [d,e,f]
> main = mainWidget $ (simpleWidgetA One) >>= display

That works, and is probably more clear than some of the earlier examples above.
But it isn't very economical/terse yet...

> simpleWidgetA' :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> simpleWidgetA' i = el "div" $ do
>   a <- liftM (const One <$>) $ button "One"
>   b <- liftM (const Two <$>) $ button "Two"
>   c <- liftM (const Three <$>) $ button "Three"
>   holdDyn i $ leftmost [a,b,c]
> main = mainWidget $ (simpleWidgetA' One) >>= display

> simpleWidgetA'' :: MonadWidget t m => Simple -> m (Dynamic t Simple)
> simpleWidgetA'' i = el "div" $ do
>   a <- mapM (\x -> liftM (x <$) (button (show x))) [One,Two,Three]
>   holdDyn i $ leftmost a
> main = mainWidget $ (simpleWidgetA'' One)  >>= display

Still think there must be a better way to do this...
