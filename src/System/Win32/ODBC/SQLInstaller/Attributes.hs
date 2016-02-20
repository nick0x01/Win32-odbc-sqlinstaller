module System.Win32.ODBC.SQLInstaller.Attributes where

class ConfigDsnAttribute a where
  toConfigDsnAttribute :: a -> (String, String)
