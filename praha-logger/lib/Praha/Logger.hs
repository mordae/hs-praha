-- |
-- Module      :  Praha.Logger
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides the 'LoggerT' transformer and an associated
-- 'MonadLogger' class to help you integrate your custom monad transformer
-- stack with "System.Log.FastLogger".
--
-- To include it in your custom transformer stack:
--
-- @
-- newtype LoggingApp a
--   = LoggingApp
--     { stack :: 'LoggerT' IO a
--     }
--   deriving (Functor, Applicative, Monad, MonadIO, 'MonadLogger')
-- @
--
-- To use it afterwards:
--
-- @
-- countFrobs :: LoggingApp Int
-- countFrobs = do
--   nfrobs \<- length \<$\> listFrobs
--
--   if (0 == nfrobs)
--      then 'logWarning' [\"There are no frobs!\"]
--      else 'logInfo' [\"There are \", 'toLogStr' nfrobs, \" frobs.\"]
--
--   return nfrobs
-- @
--
-- To execute it with the rest of your stack:
--
-- @
-- 'withFastLogger' ('LogStderr' 'defaultBufSize') \\logger ->
--   let logFunc = 'simpleLogFunc' 'LogInfo' logger
--    in 'runLoggerT' logFunc loggingApp
-- @
--

module Praha.Logger
  ( LogLevel(..)
  , MonadLogger(..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  , LoggerT
  , runLoggerT
  , simpleLogFunc

    -- * @fast-logger@
    -- | Re-exported from "System.Log.FastLogger":
  , LogStr
  , ToLogStr
  , toLogStr
  )
where
  import Praha
  import Praha.Config.Environment

  import System.Log.FastLogger


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

    showParam LogDebug   = "debug"
    showParam LogInfo    = "info"
    showParam LogWarning = "warning"
    showParam LogError   = "error"


  -- |
  -- Class for monads equipped with "System.Log.FastLogger".
  --
  class (MonadIO m) => MonadLogger m where
    -- |
    -- Return the logging function itself.
    --
    askLogger :: m LogFunc

    default askLogger :: (MonadTrans t, MonadLogger n, m ~ t n) => m LogFunc
    askLogger = lift askLogger
    {-# INLINE askLogger #-}


  instance (MonadIO m) => MonadLogger (ReaderT LogFunc m) where
    askLogger = ask


  type LogFunc = LogLevel -> LogStr -> LogStr -> IO ()


  -- |
  -- Log a single message at given level.
  --
  logMessage :: (MonadLogger m) => LogLevel -> LogStr -> LogStr -> m ()
  logMessage l t s = do
    logger <- askLogger
    liftIO $ logger l t s


  -- |
  -- Log a debugging message.
  --
  -- Example:
  --
  -- @
  -- let tag = "frobnicator"
  -- nfrobs <- frobnicateInt
  -- logDebug tag ["Counter: ", toLogStr nfrobs]
  -- @
  --
  logDebug :: (MonadLogger m) => LogStr -> [LogStr] -> m ()
  logDebug tag logStrings = logMessage LogDebug tag (mconcat logStrings)


  -- |
  -- Log an informative message.
  --
  logInfo :: (MonadLogger m) => LogStr -> [LogStr] -> m ()
  logInfo tag logStrings = logMessage LogInfo tag (mconcat logStrings)


  -- |
  -- Log a warning message.
  --
  logWarning :: (MonadLogger m) => LogStr -> [LogStr] -> m ()
  logWarning tag logStrings = logMessage LogWarning tag (mconcat logStrings)


  -- |
  -- Log an error message.
  --
  logError :: (MonadLogger m) => LogStr -> [LogStr] -> m ()
  logError tag logStrings = logMessage LogError tag (mconcat logStrings)


  -- |
  -- The logger monad transformer, which adds logging capability to
  -- the given monad. Makes use of 'ReaderT' under the wraps.
  --
  newtype LoggerT m a
    = LoggerT
      { reader         :: ReaderT LogFunc m a
      }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             , MonadFail
             , Contravariant
             , MonadZip
             , Alternative
             , MonadPlus
             , MonadIO
             , MonadUnliftIO
             , MonadTrans
             )

  instance (MonadIO m) => MonadLogger (LoggerT m) where
    askLogger = LoggerT askLogger


  -- |
  -- Run the logging monad transformer.
  --
  runLoggerT :: LogFunc -> LoggerT m a -> m a
  runLoggerT logFunc LoggerT{reader} = runReaderT reader logFunc


  -- |
  -- Creates simple log function that silences messages with
  -- level lower than the given one and prefixes all messages
  -- with their respective log levels.
  --
  -- Example:
  --
  -- @
  -- 'withFastLogger' ('LogStderr' 'defaultBufSize') \logger ->
  --   let logFunc = simpleLogFunc LogInfo logger
  --    in runLoggerT logFunc yourLoggingAppM
  -- @
  --
  simpleLogFunc :: LogLevel -> FastLogger -> LogFunc
  simpleLogFunc limit logger level tag str = do
    if level >= limit
       then logger $ mconcat [strLevel level, " (", tag, ") ", str, "\n"]
       else return ()


  -- |
  -- Log strings corresponding to the individual log levels.
  -- Used by the 'simpleLogFunc'.
  --
  strLevel :: LogLevel -> LogStr
  strLevel LogDebug   = "D"
  strLevel LogInfo    = "I"
  strLevel LogWarning = "W"
  strLevel LogError   = "E"


-- vim:set ft=haskell sw=2 ts=2 et:
