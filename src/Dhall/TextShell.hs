{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.TextShell (main) where

import Control.Exception (Handler (..), SomeException)
import Control.Monad (foldM)
import Control.Applicative (optional)
import Data.Text (Text)
import Data.Void (Void)
import Dhall.Core (Expr(Annot))
import Dhall.Import (SemanticCacheMode (..), Imported(..))
import Dhall.Parser (Src)
import Dhall.TypeCheck (Censored (..), DetailedTypeError (..), TypeError)
import Dhall.Util (Input (..), Output (..), Censor(..))
import Options.Applicative (Parser)
import System.Exit (ExitCode, exitFailure)
import qualified Control.Exception
import qualified Data.Map
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified Dhall.Util
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified System.FilePath
import qualified System.IO
import qualified System.Process

-- | Options from general dhall tools
data AsCommand = AsCommand
    { censor  :: Censor
    , explain :: Bool
    }

-- | Options specifically for TextShell
data Options = Options
    { file    :: Input
    , output  :: Output
    , argCmds :: [String]
    }

-- | Parse 'Options' and 'AsCommand'
parseConfig :: Parser (Options, AsCommand)
parseConfig = (,) <$> parseOptions
                  <*> parseAsCommand
  where
    switch name description =
        Options.Applicative.switch
            (   Options.Applicative.long name
            <>  Options.Applicative.help description
            )
    parseOptions =
      Options   <$> parseFile
                <*> parseOutput
                <*> Options.Applicative.many parseArgCmd
    parseFile = fmap f (optional p)
      where
        f  Nothing    = StandardInput
        f (Just file) = InputFile file

        p = Options.Applicative.strOption
                (   Options.Applicative.long "file"
                <>  Options.Applicative.help "Read expression from a file instead of standard input"
                <>  Options.Applicative.metavar "FILE"
                <>  Options.Applicative.action "file"
                )
    parseOutput = fmap f (optional p)
      where
        f Nothing = StandardOutput
        f (Just file) = OutputFile file

        p = Options.Applicative.strOption
                (   Options.Applicative.long "output"
                <>  Options.Applicative.help "Write result to a file instead of standard output"
                <>  Options.Applicative.metavar "FILE"
                <>  Options.Applicative.action "file"
                )
    parseArgCmd = Options.Applicative.strOption
            (   Options.Applicative.long "argCmd"
            <>  Options.Applicative.help "Use shell command to supply as `Text -> Text` argument"
            <>  Options.Applicative.metavar "CMD"
            <>  Options.Applicative.action "CMD"
            )
    parseAsCommand =
      AsCommand <$> parseCensor
                <*> switch "explain" "Explain error messages in more detail"
    parseCensor = fmap f (switch "censor" "Hide source code in error messages")
      where
        f True  = Censor
        f False = NoCensor

main :: IO ()
main = do
    (options, ac) <- Options.Applicative.execParser $
      Options.Applicative.info
          (Options.Applicative.helper <*> parseConfig)
          (   Options.Applicative.progDesc "render dhall text with shell commands as function arguments"
          <>  Options.Applicative.fullDesc
          )
    runWithOptions ac options

runWithOptions :: AsCommand -> Options -> IO ()
runWithOptions ac Options{..} = asCommand ac $ \getExpression rootDirectory -> do
    expression <- getExpression file

    resolvedExpression <-
        Dhall.Import.loadRelativeTo (rootDirectory file) UseSemanticCache expression

    let addPiLayer :: Expr Src Void -> Expr Src Void
        addPiLayer = Dhall.Core.Pi
          Nothing "_"
          (Dhall.Core.Pi Nothing "_" Dhall.Core.Text Dhall.Core.Text)
        expectedType = iterate addPiLayer Dhall.Core.Text !! length argCmds
    _ <- Dhall.Core.throws (Dhall.TypeCheck.typeOf (Annot resolvedExpression expectedType))

    let normalizedExpression = Dhall.Core.normalize resolvedExpression
        peelArg :: (Expr Void Void, Data.Map.Map Text [String])
                -> String
                -> Maybe (Expr Void Void, Data.Map.Map Text [String])
        peelArg (currExp, currMap) arg = case currExp of
          Dhall.Core.Lam _ (Dhall.Core.FunctionBinding { functionBindingVariable }) subExp ->
            Just (subExp, Data.Map.insertWith (++) functionBindingVariable [arg] currMap)
          _ -> Nothing
        peeledExprAndMap =
            foldM peelArg (normalizedExpression, Data.Map.empty) argCmds

    case peeledExprAndMap of
      Nothing -> pure () -- this should have been caught during the typecheck
      Just (expr, argMap) -> do
        res <- Dhall.Core.normalizeWithM
          (\x -> case x of
            Dhall.Core.App (Dhall.Core.Var (Dhall.Core.V v i))
                  (Dhall.Core.TextLit (Dhall.Core.Chunks [] txt))
              | Just as <- Data.Map.lookup v argMap
              , a:_     <- drop i as
              -> Just <$> do
                sysOut <- Data.Text.pack <$> System.Process.readCreateProcess
                  (System.Process.shell a)
                  (Data.Text.unpack txt)
                pure $ Dhall.Core.TextLit (Dhall.Core.Chunks [] sysOut)
            _ -> pure Nothing
          )
          expr
        case res of
          Dhall.Core.TextLit (Dhall.Core.Chunks [] text) ->
              let write = case output of
                    StandardOutput -> Data.Text.IO.putStr
                    OutputFile file_ -> Data.Text.IO.writeFile file_
              in write text
          _ -> do
              let invalidDecoderExpected :: Expr Void Void
                  invalidDecoderExpected = Dhall.Core.Text

              let invalidDecoderExpression :: Expr Void Void
                  invalidDecoderExpression = res

              Control.Exception.throwIO (Dhall.InvalidDecoder {..})

-- | Copy as much as possible the setup in "Dhall.Main".  If that module
-- changes, this should update as well.
asCommand
    :: AsCommand
    -> ((Input -> IO (Expr Src Dhall.Core.Import)) -> (Input -> FilePath) -> IO ())
    -> IO ()
asCommand AsCommand{..} act = do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    let rootDirectory = \case
            InputFile f   -> System.FilePath.takeDirectory f
            StandardInput -> "."

    let getExpression = Dhall.Util.getExpression censor

    let handle io =
            Control.Exception.catches io
                [ Handler handleTypeError
                , Handler handleImported
                , Handler handleExitCode
                ]
          where
            handleAll e = do
                let string = show (e :: SomeException)

                if not (null string)
                    then System.IO.hPutStrLn System.IO.stderr string
                    else return ()

                System.Exit.exitFailure

            handleTypeError e = Control.Exception.handle handleAll $ do
                let _ = e :: TypeError Src Void
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then
                        case censor of
                            Censor   -> Control.Exception.throwIO (CensoredDetailed (DetailedTypeError e))
                            NoCensor -> Control.Exception.throwIO (DetailedTypeError e)

                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        case censor of
                            Censor   -> Control.Exception.throwIO (Censored e)
                            NoCensor -> Control.Exception.throwIO e

            handleImported (Imported ps e) = Control.Exception.handle handleAll $ do
                let _ = e :: TypeError Src Void
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handleExitCode e =
                Control.Exception.throwIO (e :: ExitCode)

    handle $ act getExpression rootDirectory

