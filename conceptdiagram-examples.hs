{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad (zipWithM)
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Diagrams.Backend.SVG

data LabelProps = LabelProps
    { lFontSize :: Double
    , lStrutWidth :: Double
    , lStrutHeight :: Double
    }

data CurveProps = CurveProps
    { cLabelProps :: LabelProps
    , cLabelSuperscripting :: Double
    , cWidth :: Double
    , cHeight :: Double
    }

label' :: LabelProps -> String -> Diagram B R2
label' p l = text l # fontSizeL (lFontSize p) # font "sans-serif"
    <> strutX (lStrutWidth p)
    <> strutY (lStrutHeight p)

label :: String -> Diagram B R2
label = label' (LabelProps 1 2 1)

arrowLabel :: String -> Diagram B R2
arrowLabel = label' (LabelProps 0.75 2 1)

curve :: Double -> Double -> Diagram B R2
curve x y = roundedRect x y 0.5

namedCurve' :: String -> CurveProps -> Diagram B R2
namedCurve' l p = curve (cWidth p) (cHeight p) ||| lab
  where
   lab = label' (cLabelProps p) l # translateY (cLabelSuperscripting p)

namedCurve :: String -> Diagram B R2
namedCurve l = namedCurve' l props
  where
    props = CurveProps
     { cLabelProps = LabelProps 1 2 1
     , cLabelSuperscripting = 1.5
     , cWidth = 6
     , cHeight = 3
     }

subsumedCurve :: String -> Diagram B R2
subsumedCurve l = namedCurve' l props
  where
    props = CurveProps
     { cLabelProps = LabelProps 0.75 2.5 1
     , cLabelSuperscripting = 1
     , cWidth = 3
     , cHeight = 2.5
     }

mkArrow = connectOutside' (with & arrowShaft .~ shaft)
  where
    shaft = arc (0 @@ turn) (1/4 @@ turn) # reverseTrail

curveDog :: Diagram B R2
curveDog = namedCurve "Dog" # named "dog"

curveTail :: Diagram B R2
curveTail = namedCurve "Tail" # named "tail"

diagramDogSubsumePuppy :: Diagram B R2
diagramDogSubsumePuppy =
    curveDog `atop` (curvePuppy # translateX (-1.25))
  where
    curvePuppy = subsumedCurve "Puppy"

diagramDogWagTail1 :: Diagram B R2
diagramDogWagTail1 =
    curves # mkArrow "dog" "tail"
  where
    wagLabel = arrowLabel "wag" # translateY 1.25
    curves = hcat [ curveDog, wagLabel, curveTail ]

diagramDogWagTail2 :: Diagram B R2
diagramDogWagTail2  =
    curves # mkArrow "dog" "tail'"
  where
    wagLabel = arrowLabel "wag" # translateY 1.25
    curves = hcat [ curveDog, wagLabel, curveTail `atop` curveTail' ]
    curveTail' = curve 5 2.5 # named "tail'"

diagrams :: [(Diagram B R2, String)]
diagrams =
    [ (curveDog, "curve")
    , (diagramDogSubsumePuppy, "subsumption")
    , (diagramDogWagTail1, "simple-property")
    , (diagramDogWagTail2, "avf-property")
    ]

everything :: Diagram B R2
everything = vcat' (with & sep .~ 2) (map fst diagrams)

main :: IO ()
main =
    mapM_ mkSvg $ (everything, "all") : diagrams
  where
    mkSvg (d,fp) = renderSVG ("diagram-" <> fp <> ".svg") spec d
    spec = mkSizeSpec (Just 400) Nothing
