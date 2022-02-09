-- |
-- Module      :  Praha.Types
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Praha.Types
  ( -- * @base@
    -- ** Types
    -- *** @Bool@
    -- | Re-exported from "Data.Bool":
    Data.Bool.Bool(..)
    -- *** @Char@ (@String@)
    -- | Re-exported from "Data.Char":
  , Data.Char.Char
    -- | Re-exported from "Data.String":
  , Data.String.String
    -- | Re-exported from "System.IO":
  , System.IO.FilePath
    -- *** @Ordering@
    -- | Re-exported from "Data.Ord":
  , Data.Ord.Ordering(..)
    -- *** Numbers
    -- **** @Int@
    -- | Re-exported from "Data.Int":
  , Data.Int.Int
    -- ***** @Int8@
  , Data.Int.Int8
    -- ***** @Int16@
  , Data.Int.Int16
    -- ***** @Int32@
  , Data.Int.Int32
    -- ***** @Int64@
  , Data.Int.Int64
    -- **** @Word@
    -- | Re-exported from "Data.Word":
  , Data.Word.Word
    -- ***** @Word8@
  , Data.Word.Word8
    -- ***** @Word16@
  , Data.Word.Word16
    -- ***** @Word32@
  , Data.Word.Word32
    -- ***** @Word64@
  , Data.Word.Word64
    -- **** @Integer@
    -- | Re-exported from "Prelude.Integer":
  , Prelude.Integer
    -- **** @Natural@
    -- | Re-exported from "Numeric.Natural":
  , Numeric.Natural.Natural
    -- **** @Rational@
    -- | Re-exported from "Data.Ratio":
  , Data.Ratio.Rational
    -- **** @Float@
    -- | Re-exported from "Prelude":
  , Prelude.Float
    -- **** @Double@
    -- | Re-exported from "Prelude":
  , Prelude.Double

    -- *** @Maybe@
    -- | Re-exported from "Data.Maybe":
  , Data.Maybe.Maybe(..)
    -- *** @Either@
    -- | Re-exported from "Data.Either":
  , Data.Either.Either(..)
    -- *** @NonEmpty@
    -- | Re-exported from Data.List.NonEmpty
  , Data.List.NonEmpty.NonEmpty(..)
    -- *** @Proxy@
    -- | Re-exported from "Data.Proxy":
  , Data.Proxy.Proxy(..)
    -- *** @Const@
    -- | Re-exported from "Data.Functor.Const":
  , Data.Functor.Const.Const(..)
    -- *** @Identity@
    -- | Re-exported from "Data.Functor.Identity":
  , Data.Functor.Identity.Identity(..)
    -- *** @IO@
    -- | Re-exported from "System.IO":
  , System.IO.IO
    -- *** @ST@
    -- | Re-exported from "Control.Monad.ST":
  , Control.Monad.ST.ST
    -- *** @MonadIO@
    -- | Re-exported from "Control.Monad.IO.Class":
  , Control.Monad.IO.Class.MonadIO
    -- *** @MonadUnliftIO@
    -- | Re-exported from "Control.Monad.IO.Unlift":
  , Control.Monad.IO.Unlift.MonadUnliftIO

    -- ** Type Classes

    -- *** @Eq@
    -- | Re-exported from "Data.Eq":
  , Data.Eq.Eq

    -- *** @Ord@
    -- | Re-exported from "Data.Ord":
  , Data.Ord.Ord

    -- *** @Bounded@
    -- | Re-exported from "Prelude":
  , Prelude.Bounded

    -- *** @Enum@
    -- | Re-exported from "Prelude":
  , Prelude.Enum

    -- *** @HasField@
    -- | Re-exported from "GHC.Records":
  , GHC.Records.HasField

    -- *** Strings
    -- **** @Show@
    -- | Re-exported from "Text.Show":
  , Text.Show.Show
    -- **** @Read@
    -- | Re-exported from "Text.Read":
  , Text.Read.Read
    -- **** @IsString@
    -- | Re-exported from "Data.String":
  , Data.String.IsString

    -- *** List-like
    -- **** @IsList@
    -- | Re-exported from "GHC.Exts.IsList":
  , GHC.Exts.IsList

    -- *** Numeric
    -- | All numeric classes are re-exported from "Prelude":

    -- **** @Num@
  , Prelude.Num
    -- **** @Fractional@
  , Prelude.Fractional
    -- **** @Floating@
  , Prelude.Floating
    -- **** @Real@
  , Prelude.Real
    -- **** @Integral@
  , Prelude.Integral
    -- **** @RealFrac@
  , Prelude.RealFrac
    -- **** @RealFloat@
  , Prelude.RealFloat

    -- *** Categories
    -- **** @Functor@
    -- | Re-exported from "Data.Functor":
  , Data.Functor.Functor
    -- **** @Contravariant@
    -- | Re-exported from "Data.Functor.Contravariant":
  , Data.Functor.Contravariant.Contravariant
    -- **** @Foldable@
    -- | Re-exported from "Data.Foldable":
  , Data.Foldable.Foldable
    -- **** @Semigroup@
    -- | Re-exported from "Data.Semigroup":
  , Data.Semigroup.Semigroup
    -- **** @Monoid@
    -- | Re-exported from "Data.Monoid":
  , Data.Monoid.Monoid
    -- **** @Applicative@
    -- | Re-exported from "Control.Applicative":
  , Control.Applicative.Applicative
    -- **** @Alternative@
    -- | Re-exported from "Control.Applicative":
  , Control.Applicative.Alternative
    -- **** @Traversable@
    -- | Re-exported from "Data.Traversable":
  , Data.Traversable.Traversable
    -- **** @Monad@
    -- | Re-exported from "Control.Monad":
  , Control.Monad.Monad
    -- **** @MonadPlus@
    -- | Re-exported from "Control.Monad":
  , Control.Monad.MonadPlus
    -- **** @Category@
    -- | Re-exported from "Control.Category":
  , Control.Category.Category
    -- **** @Arrow@
    -- | Re-exported from "Control.Arrow":
  , Control.Arrow.Arrow
    -- **** @MonadFail@
    -- | Re-exported from "Control.Monad.Fail":
  , Control.Monad.Fail.MonadFail
    -- **** @MonadFix@
    -- | Re-exported from "Control.Monad.Fix":
  , Control.Monad.Fix.MonadFix
    -- **** @MonadZip@
    -- | Re-exported from "Control.Monad.Zip":
  , Control.Monad.Zip.MonadZip

    -- *** Data
    -- **** @Typeable@
    -- | Re-exported from "Control.Monad":
  , Data.Typeable.Typeable
    -- **** @Generic@
    -- | Re-exported from "GHC.Generics":
  , GHC.Generics.Generic

    -- **** @Kind@
    -- | Re-exported from "Data.Kind":
  , Data.Kind.Type
  , Data.Kind.Constraint

    -- *** Exceptions
    -- *** @Exception@
    -- | Re-exported from "Control.Exception.Base":
  , Control.Exception.Base.Exception
    -- **** @HasCallStack@
    -- | Re-exported from "GHC.Stack":
  , GHC.Stack.HasCallStack

    -- * @mtl@
    -- ** @MonadTrans@
    -- | Re-exported from "Control.Monad.Reader":
  , Control.Monad.Reader.MonadTrans
    -- ** @MonadReader@
  , Control.Monad.Reader.MonadReader
    -- ** @ReaderT@ (@Reader@)
    -- | Re-exported from "Control.Monad.Reader":
  , Control.Monad.Reader.Reader
  , Control.Monad.Reader.ReaderT(ReaderT)
    -- ** @MonadState@
  , Control.Monad.State.MonadState
    -- ** @StateT@ (@State@)
    -- | Re-exported from "Control.Monad.State":
  , Control.Monad.State.State
  , Control.Monad.State.StateT(StateT)

    -- * @bytestring@
    -- ** @ByteString@
    -- | Re-exported from "Data.ByteString":
  , Data.ByteString.ByteString

    -- * @text@
    -- ** @Text@
    -- | Re-exported from "Data.Text":
  , Data.Text.Text

    -- * @vector@
    -- ** @Vector@
    -- | Boxed vector re-exported from "Data.Vector":
  , Data.Vector.Vector

    -- * @containers@
    -- ** @IntMap@
    -- | Re-exported from "Data.IntMap.Strict":
  , Data.IntMap.Strict.IntMap
    -- ** @Map@
    -- | Re-exported from "Data.Map.Strict":
  , Data.Map.Strict.Map
    -- ** @IntSet@
    -- | Re-exported from "Data.IntSet":
  , Data.IntSet.IntSet
    -- ** @Set@
    -- | Re-exported from "Data.Set":
  , Data.Set.Set
    -- ** @Seq@
    -- | Re-exported from "Data.Seq":
  , Data.Sequence.Seq

    -- * @hashable@
    -- ** @Hashable@
  , Data.Hashable.Hashable

    -- * @unordered-containers@
    -- ** @HashMap@
    -- | Re-exported from "Data.HashMap.Strict":
  , Data.HashMap.Strict.HashMap
    -- ** @HashSet@
    -- | Re-exported from "Data.HashSet":
  , Data.HashSet.HashSet

    -- * @strings-conversions@
    -- ** @ConvertibleStrings@
    -- | Re-exported from "Data.String.Conversions":
  , Data.String.Conversions.ConvertibleStrings

    -- * @deepseq*
    -- ** @NFData@
    -- | Re-exported from "Control.DeepSeq":
  , Control.DeepSeq.NFData
  , Control.DeepSeq.NFData1

    -- * @key-value@
    -- ** @KeyValue@
    -- | Re-exported from "Data.KeyValue":
  , Data.KeyValue.KeyValue
  )
where
  import qualified Control.Applicative
  import qualified Control.Arrow
  import qualified Control.Category
  import qualified Control.DeepSeq
  import qualified Control.Exception.Base
  import qualified Control.Monad
  import qualified Control.Monad.Fail
  import qualified Control.Monad.Fix
  import qualified Control.Monad.IO.Class
  import qualified Control.Monad.IO.Unlift
  import qualified Control.Monad.Reader
  import qualified Control.Monad.ST
  import qualified Control.Monad.State
  import qualified Control.Monad.Zip
  import qualified Data.Bool
  import qualified Data.ByteString (ByteString)
  import qualified Data.Char
  import qualified Data.Either
  import qualified Data.Eq
  import qualified Data.Foldable
  import qualified Data.Functor
  import qualified Data.Functor.Const
  import qualified Data.Functor.Contravariant
  import qualified Data.Functor.Identity
  import qualified Data.Hashable
  import qualified Data.HashMap.Strict
  import qualified Data.HashSet
  import qualified Data.Int
  import qualified Data.IntMap.Strict
  import qualified Data.IntSet
  import qualified Data.KeyValue
  import qualified Data.Kind
  import qualified Data.List.NonEmpty
  import qualified Data.Map.Strict
  import qualified Data.Maybe
  import qualified Data.Monoid (Monoid)
  import qualified Data.Ord
  import qualified Data.Proxy
  import qualified Data.Ratio
  import qualified Data.Semigroup (Semigroup)
  import qualified Data.Sequence
  import qualified Data.Set
  import qualified Data.String.Conversions
  import qualified Data.String (IsString, String)
  import qualified Data.Text (Text)
  import qualified Data.Traversable
  import qualified Data.Typeable
  import qualified Data.Vector
  import qualified Data.Word
  import qualified GHC.Exts
  import qualified GHC.Generics
  import qualified GHC.Records
  import qualified GHC.Stack
  import qualified Numeric.Natural
  import qualified Prelude
  import qualified System.IO
  import qualified Text.Read
  import qualified Text.Show


-- vim:set ft=haskell sw=2 ts=2 et:
