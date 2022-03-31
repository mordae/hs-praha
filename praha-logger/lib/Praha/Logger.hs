-- |
-- Module      :  Praha.Logger
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- Won't output any messages below the severity specified by the @LOG_LEVEL@
-- environment variable.
--
-- or updated by the 'setMinLogLevel'. Defaults to 'LogInfo'.
--

module Praha.Logger
  (
    -- * Emitting Messages
    logDebug
  , logInfo
  , logWarning
  , logError

    -- * Composing Messages
  , ToLog(..)
  , LogBuilder

    -- * Changing Log Level
  , setMinLogLevel
  , LogLevel(..)
  )
where
  import Praha
  import Praha.Config.Environment

  import Data.ByteString.Builder
  import Data.IORef
  import System.IO (stderr)
  import System.IO.Unsafe (unsafePerformIO)

  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Lazy as LBS
  import qualified Data.ByteString.Short as SBS
  import qualified Data.Text as T
  import qualified Data.Text.Encoding as T
  import qualified Data.Text.Lazy as LT
  import qualified Data.Text.Lazy.Encoding as LT


  -- |
  -- Severity of an individual log message.
  --
  data LogLevel
    = LogDebug
    | LogInfo
    | LogWarning
    | LogError
    deriving (Show, Read, Eq, Ord, Enum, Generic)

  instance NFData LogLevel

  instance EnvParam LogLevel where
    readParam "debug"   = Just LogDebug
    readParam "info"    = Just LogInfo
    readParam "warning" = Just LogWarning
    readParam "error"   = Just LogError
    readParam _other    = Nothing
    {-# INLINE readParam #-}

    showParam LogDebug   = "debug"
    showParam LogInfo    = "info"
    showParam LogWarning = "warning"
    showParam LogError   = "error"
    {-# INLINE showParam #-}


  minLogLevel :: IORef LogLevel
  minLogLevel = unsafePerformIO $ observeConfigIORef "LOG_LEVEL" LogInfo
  {-# NOINLINE minLogLevel #-}


  -- |
  -- Adjust the minimum log level below that the messages are not printed.
  --
  setMinLogLevel :: (MonadIO m) => LogLevel -> m ()
  setMinLogLevel level = setConfig "LOG_LEVEL" level


  -- |
  -- Log a single message at given level.
  --
  logMessage :: (MonadIO m) => LogLevel -> Builder -> Builder -> m ()
  logMessage l t s = do
    liftIO do
      minLevel <- readIORef minLogLevel
      when (l >= minLevel) do
        let builder = mconcat [fromLevel l, " (", t, ") ", s, "\n"]
        hPutBuilder stderr builder

  {-# INLINE logMessage #-}


  -- |
  -- Log a debugging message.
  --
  logDebug :: (MonadIO m) => Builder -> [Builder] -> m ()
  logDebug tag logStrings = logMessage LogDebug tag (mconcat logStrings)
  {-# INLINE logDebug #-}


  -- |
  -- Log an informative message.
  --
  logInfo :: (MonadIO m) => Builder -> [Builder] -> m ()
  logInfo tag logStrings = logMessage LogInfo tag (mconcat logStrings)
  {-# INLINE logInfo #-}


  -- |
  -- Log a warning message.
  --
  logWarning :: (MonadIO m) => Builder -> [Builder] -> m ()
  logWarning tag logStrings = logMessage LogWarning tag (mconcat logStrings)
  {-# INLINE logWarning #-}


  -- |
  -- Log an error message.
  --
  logError :: (MonadIO m) => Builder -> [Builder] -> m ()
  logError tag logStrings = logMessage LogError tag (mconcat logStrings)
  {-# INLINE logError #-}


  -- |
  -- Log strings corresponding to the individual log levels.
  --
  fromLevel :: LogLevel -> Builder
  fromLevel LogDebug   = "D"
  fromLevel LogInfo    = "I"
  fromLevel LogWarning = "W"
  fromLevel LogError   = "E"
  {-# INLINE fromLevel #-}


  type LogBuilder = Builder


  -- |
  -- To convert various types to 'Builder' to be passed as parameters to
  -- the logging functions.
  --
  -- If you need more control about the value encoding, e.g. to print it in
  -- hexadecimal, use "Data.ByteString.Builder" directly.
  --
  class ToLog a where
    toLog :: a -> LogBuilder

  instance ToLog Builder where
    toLog = id

  instance ToLog BS.ByteString where
    toLog = byteString

  instance ToLog LBS.ByteString where
    toLog = lazyByteString

  instance ToLog SBS.ShortByteString where
    toLog = shortByteString

  instance ToLog Char where
    toLog = charUtf8

  instance ToLog String where
    toLog = stringUtf8

  instance ToLog Int8 where
    toLog = int8Dec

  instance ToLog Int16 where
    toLog = int16Dec

  instance ToLog Int32 where
    toLog = int32Dec

  instance ToLog Int64 where
    toLog = int64Dec

  instance ToLog Int where
    toLog = intDec

  instance ToLog Integer where
    toLog = integerDec

  instance ToLog Word16 where
    toLog = word16Dec

  instance ToLog Word32 where
    toLog = word32Dec

  instance ToLog Word64 where
    toLog = word64Dec

  instance ToLog Word where
    toLog = wordDec

  instance ToLog Float where
    toLog = floatDec

  instance ToLog Double where
    toLog = doubleDec

  instance ToLog T.Text where
    toLog = T.encodeUtf8Builder

  instance ToLog LT.Text where
    toLog = LT.encodeUtf8Builder


-- vim:set ft=haskell sw=2 ts=2 et:
