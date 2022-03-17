-- |
-- Module      :  Praha.PostgreSQL.Migrate
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides simple database migrations for PostgreSQL.
--

module Praha.PostgreSQL.Migrate
  ( migrate
  )
where
  import Praha
  import Praha.Logger

  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.SqlQQ
  import Database.PostgreSQL.Simple.Types


  -- | Tag for logging.
  tag :: LogStr
  tag = "praha-migrate"


  -- |
  -- Run database migrations specified as a list of file names and their
  -- respective contents. First argument is name of the table that holds
  -- migration history. Migrations are applied in order.
  --
  -- Example:
  --
  -- @
  -- migrate "migration" [ (\"001-init.sql\", \"create table ...\")
  --                     , (\"002-add-ts.sql\", \"alter table ...\")
  --                     ] conn
  -- @
  --
  -- Or, with @file-embed@ (which sorts by filename):
  --
  -- @
  -- migrate "migration" $(embedDir "sql") conn
  -- @
  --
  -- All migrations are run in a single transaction, so that a partial
  -- success won't leave the database in a state that is unusable with
  -- both the old and the new version of the application.
  --
  migrate :: (MonadUnliftIO m, MonadLogger m)
          => QualifiedIdentifier
          -> [(FilePath, ByteString)]
          -> Connection
          -> m ()
  migrate table migrations conn = do
    withRunInIO \runInIO -> do
      withTransaction conn do
        _ <- execute conn [sql| create table if not exists ? (
                                  name varchar not null primary key,
                                  ts timestamptz not null default now()
                                );
                              |] (Only table)

        existing <- fmap fromOnly <$>
          query conn "select name from ?" (Only table)

        for_ migrations \(name, migration) -> do
          if name `elem` existing
             then do
               runInIO $ logDebug tag ["Skipping ", toLogStr name]

             else do
               runInIO $ logInfo tag ["Running ", toLogStr name]

               _ <- execute conn "insert into ? (name) values (?)" (table, name)
               _ <- execute_ conn (Query migration)

               return ()


-- vim:set ft=haskell sw=2 ts=2 et:
