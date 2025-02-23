module Juvix.Data.Migrate where

import Juvix.Prelude.Base
import Prelude (show)

newtype Migration = Migration (Maybe Migrate)
  deriving stock (Eq, Ord, Show, Data, Generic)

noMigration :: Migration
noMigration = Migration Nothing

data Migrate
  = MigrateExportConstructors
  deriving stock (Eq, Ord, Bounded, Enum, Data, Generic)

-- NOTE `migrationString x` should not contain spaces
migrateString :: (IsString str) => Migrate -> str
migrateString = \case
  MigrateExportConstructors -> "export-constructors"

instance Show Migrate where
  show = migrateString
