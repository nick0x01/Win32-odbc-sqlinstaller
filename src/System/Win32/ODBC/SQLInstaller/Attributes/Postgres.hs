module System.Win32.ODBC.SQLInstaller.Attributes.Postgres
  ( DsnPostgresAttribute (..)
  ) where

import Data.Word
import System.Win32.ODBC.SQLInstaller.Attributes

data DsnPostgresAttribute =
    BoolsAsChar Bool
  | CommLog Bool
  | ConnSettings String
  | Debug Bool
  | Description String
  | DSN String
  | ExtraSysTablePrefixes String
  | FakeOidIndex Bool
  | Fetch Int
  | Ksqo Bool
  | LFConversion Bool
  | LowerCaseIdentifier Bool
  | MaxLongVarcharSize Word32
  | MaxVarcharSize Word8
  | Optimizer Bool
  | Parse Bool
  | Password String
  | Port Int
  | Protocol String
  | ReadOnly Bool
  | RowVersioning Bool
  | Servername String
  | ShowOidColumn Bool
  | ShowSystemTables Bool
  | Socket Int
  | TextAsLongVarchar Bool
  | TrueIsMinus1 Bool
  | UniqueIndex Bool
  | UnknownsAsLongVarchar Bool
  | UpdatableCursors Bool
  | UseDeclareFetch Bool
  | Username String
  -- | AB
  -- | ByteaAsLongVarBinary
  -- | CX
  -- | SSLmode Bool
  -- | UnknownSizes
  -- | UseServerSidePrepare

instance ConfigDsnAttribute DsnPostgresAttribute where
  toConfigDsnAttribute a = case a of
    BoolsAsChar a           -> ("BoolsAsChar", showBool a)
    CommLog a               -> ("CommLog", showBool a)
    ConnSettings a          -> ("ConnSettings", a)
    Debug a                 -> ("Debug", showBool a)
    Description a           -> ("Description", a)
    DSN a                   -> ("DSN", a)
    ExtraSysTablePrefixes a -> ("ExtraSysTablePrefixes", a)
    FakeOidIndex a          -> ("FakeOidIndex", showBool a)
    Fetch a                 -> ("Fetch", show a)
    Ksqo a                  -> ("Ksqo", showBool a)
    LFConversion a          -> ("LFConversion", showBool a)
    LowerCaseIdentifier a   -> ("LowerCaseIdentifier", showBool a)
    MaxLongVarcharSize a    -> ("MaxLongVarcharSize", show a)
    MaxVarcharSize a        -> ("MaxVarcharSize", show a)
    Optimizer a             -> ("Optimizer", showBool a)
    Parse a                 -> ("Parse", showBool a)
    Password a              -> ("Password", a)
    Port a                  -> ("Port", show a)
    Protocol a              -> ("Protocol", a)
    ReadOnly a              -> ("ReadOnly", showBool a)
    RowVersioning a         -> ("RowVersioning", showBool a)
    Servername a            -> ("Servername", a)
    ShowOidColumn a         -> ("ShowOidColumn", showBool a)
    ShowSystemTables a      -> ("ShowSystemTables", showBool a)
    Socket a                -> ("Socket", show a)
    TextAsLongVarchar a     -> ("TextAsLongVarchar", showBool a)
    TrueIsMinus1 a          -> ("TrueIsMinus1", showBool a)
    UniqueIndex a           -> ("UniqueIndex", showBool a)
    UnknownsAsLongVarchar a -> ("UnknownsAsLongVarchar", showBool a)
    UpdatableCursors a      -> ("UpdatableCursors", showBool a)
    UseDeclareFetch a       -> ("UseDeclareFetch", showBool a)
    Username a              -> ("Username", a)
    where
      showBool b = case b of
        True  -> "1"
        False -> "0"
