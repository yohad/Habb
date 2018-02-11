{-# LANGUAGE Arrows #-}
module Example (

    main,

) where

    -- Control
    import Control.Applicative as Applicative
    import Control.Arrow       as Arrow

    -- Data
    import Data.Record        as Record

    -- FRP.Grapefruit
    import FRP.Grapefruit.Signal.Discrete  as DSignal
    import FRP.Grapefruit.Signal.Segmented as SSignal

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Item          as UIItem
    import Graphics.UI.Grapefruit.Circuit       as UICircuit
    import Graphics.UI.Grapefruit.Backend.Basic as BasicUIBackend

    -- |The circuit describing the whole application.
    main :: (BasicUIBackend uiBackend) => UICircuit Window uiBackend era () (DSignal era ())
    main = proc () -> do
        rec let

                title = pure "Simple"

                text = SSignal.scan "*" (const . ('*' :)) push

            X :& Closure := closure `With` X :& Push := push
                <- window `with` just pushButton
                    -< X :& Title := title `With` X :& Text := text
        returnA -< closure
