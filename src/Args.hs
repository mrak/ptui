module Args where

import Options.Applicative
import System.Environment.XDG.BaseDir
import Data.Maybe

data Args = Args
    { config :: FilePath
    } deriving Show

data CLIArgs = CLIArgs
    { cliConfig :: Maybe FilePath
    }

configArg :: Parser (Maybe String)
configArg = optional $ strOption ( long "config"
                                  <> short 'c'
                                  <> metavar "FILE"
                                  <> help "use FILE instead of the default")

options = CLIArgs <$> configArg

processArgs :: CLIArgs -> IO Args
processArgs c = do
    xdgConfig <- getUserConfigFile "ptui" "config.ini"
    pure Args { config = fromMaybe xdgConfig (cliConfig c) }

getArgs :: IO Args
getArgs = execParser parser >>= processArgs where
    parser = (info (helper <*> options) (fullDesc <> header ""))
