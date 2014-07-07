{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad (zipWithM)
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Diagrams.Backend.SVG

data LabelProps = LabelProps
    { lFontSize :: Double
    , lFontSlant :: FontSlant
    , lStrutWidth :: Double
    , lStrutHeight :: Double
    }

data CurveProps = CurveProps
    { cLabelProps :: LabelProps
    , cLabelSuperscripting :: Double
    , cWidth :: Double
    , cHeight :: Double
    }

curveLabelProps :: LabelProps
curveLabelProps = LabelProps 1 FontSlantNormal 2 1

arrowLabelProps :: LabelProps
arrowLabelProps = LabelProps 0.75 FontSlantNormal 2 1

normalCurveProps :: CurveProps
normalCurveProps = CurveProps
    { cLabelProps = curveLabelProps
    , cLabelSuperscripting = 1.5
    , cWidth = 6
    , cHeight = 3
    }

subsumedCurveProps :: CurveProps
subsumedCurveProps = CurveProps
    { cLabelProps = curveLabelProps { lFontSize = 0.75, lStrutWidth = 2 }
    , cLabelSuperscripting = 1
    , cWidth = 3
    , cHeight = 2.5
    }


label' :: LabelProps -> String -> Diagram B R2
label' p l = text l # fontSizeL (lFontSize p)
                    # fontSlant (lFontSlant p)
                    # font "sans-serif"
    <> strutX (lStrutWidth p)
    <> strutY (lStrutHeight p)

label :: String -> Diagram B R2
label = label' curveLabelProps

arrowLabel :: String -> Diagram B R2
arrowLabel = label' arrowLabelProps

curve :: Double -> Double -> Diagram B R2
curve x y = roundedRect x y 0.5

namedCurve' :: CurveProps -> String -> Diagram B R2
namedCurve' p l = curve (cWidth p) (cHeight p) ||| lab
  where
   lab = label' (cLabelProps p) l # translateY (cLabelSuperscripting p)

namedCurve :: String -> Diagram B R2
namedCurve = namedCurve' normalCurveProps

subsumedCurve :: String -> Diagram B R2
subsumedCurve = namedCurve' subsumedCurveProps

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

diagramAvfPattern :: Diagram B R2
diagramAvfPattern =
    curves # mkArrow "c1" "c2'"
  where
    wagLabel = label' aprops "property" # translateY 2.5
    curves = hcat [ curveC1, wagLabel, curveC2 `atop` curveC2' ]
    curveC1 = (namedCurve' cprops "Class1" # named "c1") `atop` universe
    curveC2 = (namedCurve' cprops "Class2" # named "c2") `atop` universe
    curveC2' = curve 4 2.5 # named "c2'"
    universe = rect 9 5 # translateX 1.5
    aprops = arrowLabelProps { lFontSlant = FontSlantItalic
                             , lStrutWidth = 5
                             }
    cprops = normalCurveProps
        { cWidth = 5
        , cLabelProps = (cLabelProps normalCurveProps) { lFontSlant = FontSlantItalic }
        , cLabelSuperscripting = 2
        }

diagrams :: [(Diagram B R2, String)]
diagrams =
    [ (curveDog, "curve")
    , (diagramDogSubsumePuppy, "subsumption")
    , (diagramDogWagTail1, "simple-property")
    , (diagramDogWagTail2, "avf-property")
    , (diagramAvfPattern, "avf-pattern")
    ]

everything :: Diagram B R2
everything = vcat' (with & sep .~ 2) (map fst diagrams)

main :: IO ()
main =
    mapM_ mkSvg $ (everything, "all") : diagrams
  where
    mkSvg (d,fp) = renderSVG ("diagram-" <> fp <> ".svg") spec d
    spec = mkSizeSpec (Just 400) Nothing
