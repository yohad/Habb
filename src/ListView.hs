{-# LANGUAGE Arrows #-}
module ListView (

    mainCircuit

) where

    -- Control
    import Control.Applicative as Applicative
    import Control.Arrow       as Arrow

    -- Data
    import Data.Foldable        as Foldable
    import Data.Sequence        as Seq
    import Data.Fraction        as Fraction
    import Data.Colour.RGBSpace as RGBSpace
    import Data.Record          as Record

    -- FRP.Grapefruit
    import FRP.Grapefruit.Signal.Discrete             as DSignal
    import FRP.Grapefruit.Signal.Incremental          as ISignal
    import FRP.Grapefruit.Signal.Incremental.Sequence as SeqISignal

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Item              as UIItem             hiding (box)
    import Graphics.UI.Grapefruit.Circuit           as UICircuit
    import Graphics.UI.Grapefruit.Backend.Basic     as BasicUIBackend
    import Graphics.UI.Grapefruit.Backend.Container as ContainerUIBackend

    -- |The circuit describing the whole application.
    mainCircuit :: (ContainerUIBackend uiBackend) =>
                   UICircuit Window uiBackend era () (DSignal era ())
    mainCircuit = proc _ -> do
        X :& Closure := closure
             `With` X
             `With` _           <- mainWindow -< X :& Title := pure "List view"
                                                      `With` X
                                                      `With` ()
        returnA -< closure where

        mainWindow = window `with` box Vertical `with` content

    content :: (ContainerUIBackend uiBackend) => UICircuit Widget uiBackend era () ()
    content = proc _ -> do
        rec let

                fruits           = ISignal.construct (Seq.singleton Grapefruit)
                                                     (fmap (Diff . Seq.singleton) atomicFruitDiffs)

                atomicFruitDiffs = DSignal.unions [insertions, deletions, shifts, updates]

                cols             = ISignal.construct (Seq.fromList [nameCol,tastinessCol])
                                                     (fmap (Diff . Seq.singleton) atomicColDiffs)

                nameCol          = Column "name" nameDisplay textCell

                nameDisplay      = \fruit -> TextCellDisplay (name fruit) (color fruit)

                tastinessCol     = Column "tastiness" tastinessDisplay progressCell

                tastinessDisplay = \fruit -> ProgressCellDisplay (tastiness fruit) Nothing

                atomicColDiffs   = colSwaps

            _          <- display    -< (fruits,cols)
            insertions <- inserter   -< ()
            deletions  <- deleter    -< ()
            shifts     <- shifter    -< ()
            updates    <- updater    -< ()
            colSwaps   <- colSwapper -< ()

        returnA -< ()

    display :: (ContainerUIBackend uiBackend) =>
               UICircuit Widget
                         uiBackend
                         era
                         (ISignal era (Seq Fruit),ISignal era (Seq (Column uiBackend Fruit)))
                         ()
    display = proc (fruits,cols) -> do
        X :& Selection := selection <- just listView -< X :& Elements := fruits
                                                          :& Columns  := cols
        X                           <- just label    -< X :& Text     := fmap (show . toList)
                                                                              selection
        returnA -< ()

    inserter :: (BasicUIBackend uiBackend) =>
                UICircuit Widget uiBackend era () (DSignal era (AtomicDiff Fruit))
    inserter = proc _ -> do
        X :& Push := push <- just pushButton -< X :& Text := pure ("Insert an apple " ++
                                                                   "at the beginning")
        returnA -< Insertion 0 (Seq.singleton Apple) <$ push

    deleter :: (BasicUIBackend uiBackend) =>
               UICircuit Widget uiBackend era () (DSignal era (AtomicDiff Fruit))
    deleter = proc _ -> do
        X :& Push := push <- just pushButton -< X :& Text := pure "Delete the second element"
        returnA -< Deletion 1 1 <$ push

    shifter :: (BasicUIBackend uiBackend) =>
               UICircuit Widget uiBackend era () (DSignal era (AtomicDiff Fruit))
    shifter = proc _ -> do
        X :& Push := push <- just pushButton -< X :& Text := pure "Swap the first two elements"
        returnA -< Shift 0 1 1 <$ push

    updater :: (BasicUIBackend uiBackend) =>
               UICircuit Widget uiBackend era () (DSignal era (AtomicDiff Fruit))
    updater = proc _ -> do
        X :& Push := push <- just pushButton -< X :& Text := pure ("Replace the third element " ++
                                                                   "with a banana")
        returnA -< Update 2 (Seq.singleton Banana) <$ push

    colSwapper :: (BasicUIBackend uiBackend) =>
                  UICircuit Widget uiBackend era ()
                                                 (DSignal era (AtomicDiff (Column uiBackend Fruit)))
    colSwapper = proc _ -> do
        X :& Push := push <- just pushButton -< X :& Text := pure "Swap columns"
        returnA -< Shift 0 1 1 <$ push

    data Fruit = Grapefruit | Apple | Banana deriving (Show)

    name :: Fruit -> String
    name Grapefruit = "grapefruit"
    name Apple      = "apple"
    name Banana     = "banana"

    tastiness :: Fruit -> Fraction
    tastiness Grapefruit = Fraction.fromPercentage 100
    tastiness Apple      = Fraction.fromPercentage 25
    tastiness Banana     = Fraction.fromPercentage 50

    color :: Fruit -> RGB Fraction
    color Grapefruit = RGB (fromFactor 1) (fromFactor 0.8) (fromFactor 0)
    color Apple      = RGB (fromFactor 1) (fromFactor 0) (fromFactor 0)
    color Banana     = RGB (fromFactor 1) (fromFactor 1) (fromFactor 0)
