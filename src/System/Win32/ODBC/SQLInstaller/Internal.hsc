{-# LANGUAGE GeneralizedNewtypeDeriving, ForeignFunctionInterface, PatternSynonyms #-}
module System.Win32.ODBC.SQLInstaller.Internal where

import Data.Bits
import Foreign
import Foreign.C.Types
import System.Win32.Types
import Text.Printf

#include <windows.h>
#include <odbcinst.h>

-- BOOL SQLConfigDataSource(
--      HWND     hwndParent,
--      WORD     fRequest,
--      LPCSTR   lpszDriver,
--      LPCSTR   lpszAttributes);
foreign import stdcall "odbcinst.h SQLConfigDataSourceW"
  c_SQLConfigDataSource :: HANDLE -> WORD -> LPTSTR -> LPTSTR -> IO BOOL

newtype OdbcConfigDsnReq = OdbcConfigDsnReq { unOdbcConfigDsnReq :: WORD }
  deriving (Bits, Eq, Show)

pattern ODBC_ADD_DSN = OdbcConfigDsnReq 0x01
pattern ODBC_CONFIG_DSN = OdbcConfigDsnReq 0x02
pattern ODBC_REMOVE_DSN = OdbcConfigDsnReq 0x03
pattern ODBC_ADD_SYS_DSN = OdbcConfigDsnReq 0x04
pattern ODBC_CONFIG_SYS_DSN = OdbcConfigDsnReq 0x05
pattern ODBC_REMOVE_SYS_DSN = OdbcConfigDsnReq 0x06

odbcConfigDsnReqNames :: [(OdbcConfigDsnReq, String)]
odbcConfigDsnReqNames =
  [ (ODBC_ADD_DSN, "ODBC_ADD_DSN")
  , (ODBC_CONFIG_DSN, "ODBC_CONFIG_DSN")
  , (ODBC_REMOVE_DSN, "ODBC_REMOVE_DSN")
  , (ODBC_ADD_SYS_DSN, "ODBC_ADD_SYS_DSN")
  , (ODBC_CONFIG_SYS_DSN, "ODBC_CONFIG_SYS_DSN")
  , (ODBC_REMOVE_SYS_DSN, "ODBC_REMOVE_SYS_DSN")
  ]
