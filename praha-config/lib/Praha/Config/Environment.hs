-- |
-- Module      :  Praha.Config.Environment
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides:
--
-- 1. Class to parse configuration parameters
-- 2. Functions to work with process environment
-- 3. Functions to update the environment from configuration files
--
-- This is a good-enough solution for typical web services, but far from
-- ideal for shell commands and similar, because it does not support any
-- kind of hierachy.
--

module Praha.Config.Environment
  ( -- * Parsing Parameter Values
    EnvParam(..)

    -- * Working With Parameters
  , getConfig
  , getConfigMaybe
  , getConfigDefault
  , setConfig
  , setConfigDefault

    -- * Observing Changes
  , observeConfig
  , observeConfigIORef

    -- * Using Files
  , readFileToEnv
  , readFileToEnvDefault
  )
where
  import Praha

  import UnliftIO
  import UnliftIO.Concurrent
  import UnliftIO.Environment

  import System.IO.Error (userError)
  import System.IO (readFile)
  import System.IO.Unsafe (unsafePerformIO)

  import Data.List (drop, span, isPrefixOf, lines, dropWhile, dropWhileEnd)
  import Data.Char (isSpace)


  changelog :: Chan (String, String)
  changelog = unsafePerformIO newChan
  {-# NOINLINE changelog #-}


  -- |
  -- Class for parameter types that can be stored as environment
  -- variable values.
  --
  class EnvParam a where
    -- |
    -- Attempt to parse the parameter value from string.
    --
    readParam :: String -> Maybe a

    -- |
    -- Format the parameter value as string.
    --
    showParam :: a -> String

  instance EnvParam String where
    readParam = Just
    showParam = id

  instance EnvParam Text where
    readParam = Just . cs
    showParam = cs

  instance EnvParam ByteString where
    readParam = Just . cs
    showParam = cs

  instance EnvParam Int where
    readParam = readMaybe
    showParam = show

  instance EnvParam Int8 where
    readParam = readMaybe
    showParam = show

  instance EnvParam Int16 where
    readParam = readMaybe
    showParam = show

  instance EnvParam Int32 where
    readParam = readMaybe
    showParam = show

  instance EnvParam Int64 where
    readParam = readMaybe
    showParam = show

  instance EnvParam Integer where
    readParam = readMaybe
    showParam = show

  instance EnvParam Word where
    readParam = readMaybe
    showParam = show

  instance EnvParam Word8 where
    readParam = readMaybe
    showParam = show

  instance EnvParam Word16 where
    readParam = readMaybe
    showParam = show

  instance EnvParam Word32 where
    readParam = readMaybe
    showParam = show

  instance EnvParam Word64 where
    readParam = readMaybe
    showParam = show

  instance EnvParam Natural where
    readParam = readMaybe
    showParam = show

  instance EnvParam Double where
    readParam = readMaybe
    showParam = show

  instance EnvParam Float where
    readParam = readMaybe
    showParam = show

  instance EnvParam () where
    readParam "" = Just ()
    readParam _  = Nothing
    showParam () = ""

  instance EnvParam Bool where
    readParam "true"  = Just True
    readParam "True"  = Just True
    readParam "yes"   = Just True
    readParam "1"     = Just True
    readParam "false" = Just False
    readParam "False" = Just False
    readParam "no"    = Just False
    readParam "0"     = Just False
    readParam _other  = Nothing
    showParam True    = "True"
    showParam False   = "False"

  instance (EnvParam a) => EnvParam (Maybe a) where
    readParam ""  = Just Nothing
    readParam str = Just (readParam str)

    showParam Nothing  = ""
    showParam (Just x) = showParam x


  -- |
  -- Find and parse the parameter or throw an 'userError'.
  --
  getConfig :: (MonadIO m, EnvParam a) => String -> m a
  getConfig name = do
    maybeValue <- lookupEnv name

    case maybeValue of
      Nothing -> throwIO $ userError $ "Variable " <> name <> " is missing."
      Just sv -> decodeParam name sv


  -- |
  -- Find and try to parse the parameter.
  -- If the parameter is not found, raise an 'userError'.
  --
  getConfigMaybe :: (MonadIO m, EnvParam a) => String -> m (Maybe a)
  getConfigMaybe name = do
    maybeValue <- lookupEnv name

    case maybeValue of
      Nothing -> return Nothing
      Just sv -> decodeParam name sv


  -- |
  -- Try to find and parse the parameter.
  -- If the parameter cannot be parsed, raise an 'userError'.
  -- If the parameter cannot be found, use the supplied default value.
  --
  getConfigDefault :: (MonadIO m, EnvParam a) => String -> a -> m a
  getConfigDefault name dfl = do
    maybeValue <- lookupEnv name

    case maybeValue of
      Nothing -> return dfl
      Just sv -> decodeParam name sv


  decodeParam :: (MonadIO m, EnvParam a) => String -> String -> m a
  decodeParam name sv = do
    case readParam sv of
      Nothing -> throwIO $ userError $ "Failed to parse " <> name <> "=" <> sv
      Just v  -> return v


  -- |
  -- Set given parameter.
  --
  setConfig :: (MonadIO m, EnvParam a) => String -> a -> m ()
  setConfig name value = do
    let text = showParam value
    setEnv name text
    writeChan changelog (name, text)


  -- |
  -- Set given parameter unless it has already been set before.
  --
  setConfigDefault :: (MonadIO m, EnvParam a) => String -> a -> m ()
  setConfigDefault name value = setEnvDefault name (showParam value)


  setEnvDefault :: (MonadIO m) => String -> String -> m ()
  setEnvDefault name value = do
    maybeValue <- lookupEnv name

    case maybeValue of
      Nothing -> setConfig name value
      Just _  -> return ()


  -- |
  -- Register a function to be called (in parallel) every time the
  -- configuration changes through 'setConfig' or 'readFileToEnv' ans so on.
  --
  -- Obviously does not track environment changes done outside this module.
  --
  observeConfig :: (MonadUnliftIO m, EnvParam a) => String -> (Maybe a -> m ()) -> m ()
  observeConfig name handler = do
    withRunInIO \runInIO -> do
      chan <- dupChan changelog
      void $ forkIO do
        forever do
          (name', text) <- readChan chan
          when (name' == name) do
            runInIO $ handler (readParam text)


  -- |
  -- Creates an 'IORef' that updates itself automatically whenever the
  -- configuration changes.
  --
  observeConfigIORef :: (MonadIO m, EnvParam a) => String -> a -> m (IORef a)
  observeConfigIORef name dfl = do
    liftIO do
      chan  <- dupChan changelog
      value <- getConfigDefault name dfl
      ioref <- newIORef value

      void $ forkIO do
        forever do
          (name', text) <- readChan chan
          when (name' == name) do
            writeIORef ioref (fromMaybe dfl $ readParam text)

      return ioref


  -- |
  -- Read a file into the environment, updating the values.
  --
  -- All empty lines as well as lines starting with @#@ are ignored.
  --
  readFileToEnv :: (MonadIO m) => FilePath -> m ()
  readFileToEnv path = do
    contents <- liftIO $ readFile path
    mapM_ (uncurry setEnv) (parse contents)


  -- |
  -- Read a file into the environment, filling in the missing values.
  --
  -- All empty lines as well as lines starting with @#@ are ignored.
  --
  readFileToEnvDefault :: (MonadIO m) => FilePath -> m ()
  readFileToEnvDefault path = do
    contents <- liftIO $ readFile path
    mapM_ (uncurry setEnvDefault) (parse contents)


  parse :: String -> [(String, String)]
  parse = mapMaybe parseLine . lines
    where
      parseLine   = fmap tidy . fmap parseKV . reject . strip
      parseKV     = fmap (drop 1) . span (/= '=')
      reject      = guarded (not . isPrefixOf "#") <=< guarded ("" /=)
      tidy (k, v) = (strip k, strip v)
      strip       = dropWhile isSpace . dropWhileEnd isSpace


-- vim:set ft=haskell sw=2 ts=2 et:
