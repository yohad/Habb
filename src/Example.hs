module Examples (

    mainCircuit,

) where

    -- Control
    import Control.Applicative as Applicative
    import Control.Arrow       as Arrow

    -- Data
    import Data.Record as Record

    -- FRP.Grapefruit
    import FRP.Grapefruit.Signal.Discrete as DSignal

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Item          as UIItem         hiding (box)
    import Graphics.UI.Grapefruit.Circuit       as UICircuit
    import Graphics.UI.Grapefruit.Backend.Basic as BasicUIBackend

    mainCircuit :: (BasicUIBackend uiBackend) => UICircuit Window uiBackend era () (DSignal era ())
    mainCircuit = proc _ -> do
        X :& Closure := closure `With` X `With` _  <- mainWindow -< X :& Title := mainWindowTitle
                                                                      `With` X `With` ()
        returnA -< closure where

        mainWindow      = window `with` box Vertical `with` content

        mainWindowTitle = pure "Converter"

    content :: (BasicUIBackend uiBackend) => UICircuit Widget uiBackend era () ()
    content = proc _ -> do
        X :& Content := decimal <- just lineEditor -< X
        X                       <- just label      -< X :& Text := decimalToBinary <$> decimal
        returnA -< ()

    decimalToBinary :: String -> String
    decimalToBinary decimal = case reads decimal of
                                  [(number,"")] -> reverse (numberToReverseBinary number)
                                  _             -> ""

    numberToReverseBinary :: Integer -> String
    numberToReverseBinary number | number == 0 = "0"
                                 | number == 1 = "1"
                                 | otherwise   = numberToReverseBinary (number `mod` 2) ++
                                                 numberToReverseBinary (number `div` 2)
