module Main where

import Options.Applicative
import System.Win32.ODBC.SQLInstaller
import System.Win32.ODBC.SQLInstaller.Attributes.Common

data Command =
    AddDsn CmdOpts
  | RemoveDsn CmdOpts

data CmdOpts = CmdOpts
  { optsDriverName :: String
  , optsDsnName    :: String
  }

parseCmdOpts :: Parser CmdOpts
parseCmdOpts = CmdOpts
 <$> strOption (long "driver")
 <*> strOption (long "dsn")

parseAddDsn :: Parser Command
parseAddDsn = AddDsn <$> parseCmdOpts

parseRemoveDsn :: Parser Command
parseRemoveDsn = RemoveDsn <$> parseCmdOpts

parseCommand :: Parser Command
parseCommand = subparser $
      command "add-dsn"     (info parseAddDsn ( progDesc "Add a new user data source." ))
  <>  command "remove-dsn"  (info parseRemoveDsn ( progDesc "Remove an existing user data source." ))

run :: Command -> IO ()
run c = case c of
  AddDsn (CmdOpts drv dsn)    -> sqlConfigDataSource' ODBC_ADD_DSN    drv [ DSN dsn ]
  RemoveDsn (CmdOpts drv dsn) -> sqlConfigDataSource' ODBC_REMOVE_DSN drv [ DSN dsn ]

main :: IO ()
main = run =<< execParser (info (helper <*> parseCommand) mempty)
