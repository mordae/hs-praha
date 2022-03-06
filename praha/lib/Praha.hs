-- |
-- Module      :  Praha
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Praha
  (
    module Praha.Types
    -- * @Bool@
    -- | Re-exported from "Data.Bool":
  , (Data.Bool.||)
  , (Data.Bool.&&)
  , Data.Bool.not
  , Data.Bool.otherwise
  , Data.Bool.bool

    -- * @Maybe@
    -- | Re-exported from "Data.Maybe":
  , Data.Maybe.maybe
  , Data.Maybe.fromMaybe
  , Data.Maybe.listToMaybe
  , Data.Maybe.maybeToList
  , Data.Maybe.catMaybes
  , Data.Maybe.mapMaybe
  , Data.Maybe.isJust
  , Data.Maybe.isNothing

    -- * @Either@
    -- | Re-exported from "Data.Either":
  , Data.Either.either
  , Data.Either.fromLeft
  , Data.Either.fromRight
  , Data.Either.lefts
  , Data.Either.partitionEithers
  , Data.Either.rights

    -- * Tuples
    -- | Re-exported from "Data.Tuple":
  , Data.Tuple.fst
  , Data.Tuple.snd
  , Data.Tuple.curry
  , Data.Tuple.uncurry

    -- * @Eq@
    -- | Re-exported from "Data.Eq":
  , (Data.Eq.==)
  , (Data.Eq./=)

    -- * @Ord@
    -- | Re-exported from "Data.Ord":
  , (Data.Ord.<)
  , (Data.Ord.<=)
  , (Data.Ord.>)
  , (Data.Ord.>=)
  , Data.Ord.max
  , Data.Ord.min
  , Data.Ord.compare
  , Data.Ord.comparing
  , Data.Ord.Down(..)

    -- * @Enum@
    -- | Re-exported from "Prelude":
  , Prelude.fromEnum

    -- * @HasField@
    -- | Re-exported from "GHC.Records":
  , GHC.Records.getField

    -- * @Bounded@
    -- | Re-exported from "Prelude":
  , Prelude.minBound
  , Prelude.maxBound

    -- * @Num@
    -- | Re-exported from "Prelude":
  , (Prelude.+)
  , (Prelude.-)
  , (Prelude.*)
  , (Prelude.^)
  , Prelude.negate
  , Prelude.abs
  , Prelude.signum
  , Prelude.fromInteger
  , Prelude.subtract

    -- * @Real@
    -- | Re-exported from "Prelude":
  , Prelude.toRational

    -- * @Integral@
    -- | Re-exported from "Prelude":
  , Prelude.quot
  , Prelude.rem
  , Prelude.div
  , Prelude.mod
  , Prelude.quotRem
  , Prelude.divMod
  , Prelude.toInteger
  , Prelude.even
  , Prelude.odd
  , Prelude.gcd
  , Prelude.lcm
  , Prelude.fromIntegral

    -- * @Fractional@
    -- | Re-exported from "Prelude":
  , (Prelude./)
  , (Prelude.^^)
  , Prelude.recip
  , Prelude.fromRational
  , Prelude.realToFrac

    -- * @Floating@
    -- | Re-exported from "Prelude":
  , Prelude.pi
  , Prelude.exp
  , Prelude.log
  , Prelude.sqrt
  , (Prelude.**)
  , Prelude.logBase
  , Prelude.sin
  , Prelude.cos
  , Prelude.tan
  , Prelude.asin
  , Prelude.acos
  , Prelude.atan
  , Prelude.sinh
  , Prelude.cosh
  , Prelude.tanh
  , Prelude.asinh
  , Prelude.acosh
  , Prelude.atanh

    -- * @RealFrac@
    -- | Re-exported from "Prelude":
  , Prelude.properFraction
  , Prelude.truncate
  , Prelude.round
  , Prelude.ceiling
  , Prelude.floor

    -- * @RealFloat@
    -- | Re-exported from "Prelude":
  , Prelude.floatRadix
  , Prelude.floatDigits
  , Prelude.floatRange
  , Prelude.decodeFloat
  , Prelude.encodeFloat
  , Prelude.exponent
  , Prelude.significand
  , Prelude.scaleFloat
  , Prelude.isNaN
  , Prelude.isInfinite
  , Prelude.isDenormalized
  , Prelude.isNegativeZero
  , Prelude.isIEEE
  , Prelude.atan2

    -- * @Word@
    -- | Re-exported from "Data.Word":
  , Data.Word.byteSwap16
  , Data.Word.byteSwap32
  , Data.Word.byteSwap64

    -- * @Semigroup@
    -- | Re-exported from "Data.Semigroup":
  , (Data.Semigroup.<>)
  , Data.Semigroup.sconcat

    -- * @Monoid@
    -- | Re-exported from "Data.Monoid":
  , Data.Monoid.mempty
  , Data.Monoid.mappend
  , Data.Monoid.mconcat

    -- * @Functor@
    -- | Re-exported from "Data.Functor":
  , Data.Functor.fmap
  , (Data.Functor.<$>)
  , (Data.Functor.<&>)
  , (Data.Functor.<$)
  , (Data.Functor.$>)
  , Data.Functor.void

    -- * @Applicative@
    -- | Re-exported from "Control.Applicative":
  , Control.Applicative.pure
  , (Control.Applicative.<*>)
  , (Control.Applicative.<*)
  , (Control.Applicative.*>)
  , Control.Applicative.liftA
  , Control.Applicative.liftA2
  , Control.Applicative.liftA3
  , Control.Monad.forever
  , Data.Foldable.traverse_
  , Data.Foldable.for_
  , Data.Foldable.sequenceA_
  , Control.Monad.filterM
  , Control.Monad.replicateM_
  , Control.Monad.zipWithM
  , Control.Monad.zipWithM_
  , Praha.Extra.guarded

    -- * @Monad@
    -- | Re-exported from "Control.Monad":
  , Control.Monad.return
  , Control.Monad.join
  , Control.Monad.fail
  , (Control.Monad.>>=)
  , (Control.Monad.>>)
  , (Control.Monad.=<<)
  , (Control.Monad.>=>)
  , (Control.Monad.<=<)
  , (Control.Monad.<$!>)
  , Control.Monad.liftM
  , Control.Monad.liftM2
  , Praha.Extra.whenM
  , Praha.Extra.unlessM
  , Data.Foldable.mapM_
  , Data.Foldable.forM_
  , Data.Foldable.sequence_
  , Control.Monad.foldM
  , Control.Monad.foldM_

    -- * @Foldable@
    -- | Re-exported from "Data.Foldable":
  , Data.Foldable.foldr
  , Data.Foldable.foldl'
  , Data.Foldable.fold
  , Data.Foldable.foldMap
  , Data.Foldable.elem
  , Data.Foldable.notElem
  , Data.Foldable.null
  , Data.Foldable.length
  , Data.Foldable.sum
  , Data.Foldable.product
  , Data.Foldable.all
  , Data.Foldable.any
  , Data.Foldable.and
  , Data.Foldable.or
  , Data.Foldable.toList
  , Data.Foldable.concat
  , Data.Foldable.concatMap

    -- * @Traversable@
    -- | Re-exported from "Data.Traversable":
  , Data.Traversable.traverse
  , Data.Traversable.for
  , Data.Traversable.sequenceA
  , Data.Traversable.mapM
  , Data.Traversable.forM
  , Data.Traversable.sequence

    -- * @Alternative@
    -- | Re-exported from "Control.Applicative":
  , (Control.Applicative.<|>)
  , Control.Applicative.some
  , Control.Applicative.many
  , Control.Applicative.optional
  , Data.Foldable.asum
  , Control.Monad.guard
  , Control.Monad.when
  , Control.Monad.unless

    -- * @MonadPlus@
    -- | Re-exported from "Control.Monad":
  , Control.Monad.mzero
  , Control.Monad.mplus
  , Control.Monad.msum
  , Control.Monad.mfilter

    -- * @Arrow@
    -- | Re-exported from "Control.Arrow" and "Control.Category":
  , (Control.Arrow.&&&)
  , (Control.Arrow.***)
  , (Control.Category.>>>)

    -- * @Function@
    -- | Re-exported from "Data.Function":
  , Data.Function.id
  , Data.Function.const
  , (Data.Function..)
  , (Data.Function.$)
  , (Data.Function.&)
  , Data.Function.flip
  , Data.Function.fix
  , Data.Function.on

    -- * @MonadIO@
    -- | Re-exported from "Control.Monad.IO.Class":
  , Control.Monad.IO.Class.liftIO

    -- * Miscellaneous functions
  , (Prelude.$!)
  , Prelude.seq
  , Prelude.error
  , Prelude.undefined
  , Prelude.asTypeOf

    -- * List
    -- | Re-exported from "Data.List":
  , Data.List.zip
  , Data.List.zipWith

    -- * @IsList@
    -- | Re-exported from "GHC.Exts.fromList":
  , GHC.Exts.fromList

    -- * @IsString@
    -- | Re-exported from "Data.String":
  , Data.String.fromString

    -- ** @Show@
    -- | Re-exported from "Text.Show":
  , Text.Show.show
    -- ** @Read@
    -- | Re-exported from "Text.Read":
  , Text.Read.readMaybe

    -- * @Reader@
    -- | Re-exported from "Control.Monad.Reader":
  , Control.Monad.Reader.lift
  , Control.Monad.Reader.ask
  , Control.Monad.Reader.asks
  , Control.Monad.Reader.local
  , Control.Monad.Reader.runReader
  , Control.Monad.Reader.runReaderT

    -- * @State@
    -- | Re-exported from "Control.Monad.State":
  , Control.Monad.State.runState
  , Control.Monad.State.runStateT
  , Control.Monad.State.evalState
  , Control.Monad.State.evalStateT
  , Control.Monad.State.execState
  , Control.Monad.State.execStateT
  , Control.Monad.State.modify
  , Control.Monad.State.gets

    -- * @Text@
  , Praha.Extra.tshow

    -- ** @ConvertibleStrings@
    -- | Re-exported from "Data.String.Conversions":
  , Data.String.Conversions.cs
  , Data.String.Conversions.convertString

    -- * @MonadUnliftIO@
    -- | Re-exported from "Control.Monad.IO.Unlift":
  , Control.Monad.IO.Unlift.withRunInIO

    -- * @NFData@
    -- | Re-exported from "Control.DeepSeq":
  , Control.DeepSeq.rnf
  , Control.DeepSeq.rnf1
  , Control.DeepSeq.liftRnf
  , Control.DeepSeq.deepseq
  , Control.DeepSeq.force
  , (Control.DeepSeq.$!!)
  , (Control.DeepSeq.<$!!>)

    -- * @KeyValue@
  , (Praha.Extra..=)
  )
where
  import Praha.Types
  import qualified Control.Applicative
  import qualified Control.Arrow
  import qualified Control.Category
  import qualified Control.DeepSeq
  import qualified Control.Monad
  import qualified Control.Monad.IO.Class
  import qualified Control.Monad.IO.Unlift
  import qualified Control.Monad.Reader
  import qualified Control.Monad.State
  import qualified Data.Bool
  import qualified Data.Either
  import qualified Data.Eq
  import qualified Data.Foldable
  import qualified Data.Function
  import qualified Data.Functor
  import qualified Data.List
  import qualified Data.Maybe
  import qualified Data.Monoid
  import qualified Data.Ord
  import qualified Data.Semigroup
  import qualified Data.String
  import qualified Data.String.Conversions
  import qualified Data.Traversable
  import qualified Data.Tuple
  import qualified Data.Word
  import qualified GHC.Exts
  import qualified GHC.Records
  import qualified Praha.Extra
  import qualified Prelude
  import qualified Text.Read
  import qualified Text.Show

-- vim:set ft=haskell sw=2 ts=2 et:
