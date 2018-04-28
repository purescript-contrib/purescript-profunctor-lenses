module PrismsForSumTypes where

{-   If you want to try out examples, paste the following into the repl.

import PrismsForSumTypes
import Data.Lens
import Data.Lens.Prism
import Color as Color
import Data.Maybe
import Data.Record.ShowRecord (showRecord)

Examples are written in this format:

s1 :: Maybe Color
s1 = preview solidFocus (Solid Color.white) -- (Just rgba 255 255 255 1.0)

That's so that a typical syntax highlighter will make the executable code
easy to spot among the commentary. (The name-value binding is unfortunate,
but needed to prevent noise from compiler warnings.)

-}

import Prelude
import Data.Lens (Prism', is, isn't, nearly, only, preview, prism, prism', review)

import Color (Color)
import Color as Color

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow
import Data.Record.ShowRecord (showRecord)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))


                {- The types in question -}

newtype Percent = Percent Number
data Point = Point Number Number
  
data Fill
  = Solid Color
  | LinearGradient Color Color Percent
  | RadialGradient Color Color Point
  | NoFill

                {------ Some samples to work with ------}

fillWhite :: Fill
fillWhite = Solid Color.white

fillBlackToWhite :: Fill
fillBlackToWhite = LinearGradient Color.black Color.white $ Percent 3.3

fillWhiteToBlack :: Fill
fillWhiteToBlack = LinearGradient Color.white Color.black $ Percent 3.3

fillRadial :: Fill
fillRadial = RadialGradient Color.white Color.black $ Point 1.0 3.4


                {------ Eq and Show come in handy ------}    

derive instance genericPercent :: Generic Percent _
instance eqPercent :: Eq Percent where
  eq = GEq.genericEq
instance showPercent :: Show Percent where
  show (Percent f) = "(" <> show f <> "%)"

derive instance genericPoint :: Generic Point _
instance eqPoint :: Eq Point where
  eq = GEq.genericEq
instance showPoint :: Show Point where
  show (Point x y) = "(" <> show x <> ", " <> show y <> ")"

derive instance genericFill :: Generic Fill _
instance eqFill :: Eq Fill where
  eq = GEq.genericEq
instance showFill :: Show Fill where
  show x = GShow.genericShow x


                {------ Making prisms with Maybe and `prism'` ------}
                {------ Basic usage ------}

-- Two function arguments: a data constructor for the type in
-- question, plus one that "substitutes" your desired case with `Just`
-- and converts all other values to `Nothing`.

solidFocus :: Prism' Fill Color
solidFocus = prism' constructor focus
  where
    constructor = Solid
    focus fill = case fill of
      Solid color -> Just color
      _ -> Nothing

-- In real life, you might abbreviate the above to this:

solidFocus' :: Prism' Fill Color
solidFocus' = prism' Solid case _ of 
  Solid color -> Just color
  _ -> Nothing

-- ... but being painfully explicit is better for examples.

-- After building a prism, you focus in on a color with `preview`:

s1 :: Maybe Color
s1 = preview solidFocus (Solid Color.white) -- (Just rgba 255 255 255 1.0)

s2 :: Maybe Color
s2 = preview solidFocus fillRadial -- Nothing

-- ... or you can create a Fill from a color with `review`:

s3 :: Fill
s3 = review solidFocus Color.white
-- (Solid rgba 255 255 255 1.0)

-- ... or you can ask whether a given value matches the prism:

s4 :: Boolean
s4 = is solidFocus fillWhite :: Boolean -- true

s5 :: Boolean
s5 = isn't solidFocus fillWhite :: Boolean -- false



                {------ Making prisms with Either and `prism` ------}

-- Since `LinearGradient` wraps multiple values, they need to be
-- rewrapped for `preview`. I'll use a record.


type LinearInterchange =
  { color1 :: Color
  , color2 :: Color
  , percent :: Percent
  }

-- When making a prism with `prism`, the "focus" function returns
-- either the selected value (as `Right`) or the entire argument (as
-- `Left`).

linearFocus :: Prism' Fill LinearInterchange
linearFocus = prism constructor focus
  where
    constructor {color1, color2, percent} =
      LinearGradient color1 color2 percent
    focus = case _ of
      LinearGradient color1 color2 percent ->
        Right  {color1, color2, percent}
      fill ->
        Left fill
      
-- Even though made differently, this prism is used the same way:

l1 :: String
l1 = preview linearFocus fillBlackToWhite # maybe "!" showRecord
-- "{ color1: rgba 0 0 0 1.0, color2: rgba 255 255 255 1.0, percent: (3.3%) }"

l2 :: Fill
l2 = review linearFocus { color1 : Color.black
                        , color2 : Color.white
                        , percent : Percent 33.3
                        }

                {------ Constructing more specific prisms ------}

-- `only` is used to check for a specific value:

whiteSolid :: Prism' Fill Unit
whiteSolid = only (Solid Color.white)

o1 :: Boolean
o1 = is whiteSolid (Solid Color.white) :: Boolean -- true

o2 :: Boolean
o2 = is whiteSolid (Solid Color.black) :: Boolean -- false

o3 :: Boolean
o3 = is whiteSolid fillRadial :: Boolean -- false


-- `nearly` is typically used to look for a specific case (like other
-- prisms), but accepting only values that are close to some target
-- value. It takes two values: a reference value (which, with sum types, 
-- serves to declare the focus case, and a predicate that determines
-- whether the wrapped value(s) is close enough to what's desired.

-- In this example, we want to focus on solid colors that are "bright
-- enough". 

brightSolid :: Prism' Fill Unit
brightSolid = nearly (Solid referenceColor) predicate
  where
    referenceColor = Color.graytone 0.8
    predicate = case _ of
      Solid color -> 
        Color.brightness color >= Color.brightness referenceColor
      _ ->
        false

-- Because a `nearly` prism focuses on `Unit`, you get only two values
-- from `preview`:


n1 :: Maybe Unit
n1 = preview brightSolid (Solid Color.white) -- (Just unit)

n2 :: Maybe Unit
n2 = preview brightSolid (Solid Color.black) --  Nothing

n3 :: Maybe Unit
n3 = preview brightSolid NoFill --  Nothing

      
-- So you probably want to use `is` or `isn't`:

n4 :: Boolean
n4 = is brightSolid (Solid Color.white) :: Boolean -- true

-- You can recover the reference value with `review`:

n5 :: Fill
n5 = review brightSolid unit -- (Solid rgba 204 204 204 1.0)
