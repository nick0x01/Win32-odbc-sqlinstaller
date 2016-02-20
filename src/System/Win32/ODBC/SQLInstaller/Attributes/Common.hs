module System.Win32.ODBC.SQLInstaller.Attributes.Common
  ( CommonDsnAttribut (..)
  ) where

import System.Win32.ODBC.SQLInstaller.Attributes

data CommonDsnAttribut =
    DSN String
  | FILEDSN FilePath
  | DRIVER String
  | UID String
  | PWD String
  | SAVEFILE FilePath

instance ConfigDsnAttribute CommonDsnAttribut where
  toConfigDsnAttribute a = case a of
    DSN a       -> ("DSN", a)
    FILEDSN a   -> ("FILEDSN", a)
    DRIVER a    -> ("DRIVER", a)
    UID a       -> ("UID", a)
    PWD a       -> ("PWD", a)
    SAVEFILE a  -> ("SAVEFILE", a)
