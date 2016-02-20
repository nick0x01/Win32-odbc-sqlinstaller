{-# LANGUAGE PatternSynonyms #-}
module System.Win32.ODBC.SQLInstaller
  ( sqlConfigDataSource
  , sqlConfigDataSource'
  , OdbcConfigDsnReq (..)
  , pattern ODBC_ADD_DSN
  , pattern ODBC_CONFIG_DSN
  , pattern ODBC_REMOVE_DSN
  , pattern ODBC_ADD_SYS_DSN
  , pattern ODBC_CONFIG_SYS_DSN
  , pattern ODBC_REMOVE_SYS_DSN
  ) where

import Foreign
import Foreign.C.Types
import System.Win32.ODBC.SQLInstaller.Internal
import System.Win32.ODBC.SQLInstaller.Attributes
import System.Win32.Types

sqlConfigDataSource :: (ConfigDsnAttribute attr) => HANDLE -> OdbcConfigDsnReq -> String -> [attr] -> IO ()
sqlConfigDataSource hndl req drv attrs =
  withTString (concatAttrs attrs) $ \lpszAttributes ->
  withTString drv $ \lpszDriver ->
    failIfFalse_ "SQLConfigDataSourceW" $ c_SQLConfigDataSource hndl (unOdbcConfigDsnReq req) lpszDriver lpszAttributes
  where
    concatAttrs xs =
      let addNull x = x ++ "\0"
          attrStr (n, v) = addNull $ concat [n, "=", v]
      in addNull $ concatMap (attrStr .  toConfigDsnAttribute) xs

sqlConfigDataSource' :: (ConfigDsnAttribute attr) => OdbcConfigDsnReq -> String -> [attr] -> IO ()
sqlConfigDataSource' = sqlConfigDataSource nullPtr
