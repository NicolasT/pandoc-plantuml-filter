{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (unless)

import System.IO (hClose, hPutStrLn, IOMode(..), stderr, withFile)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.Process (CreateProcess(std_in, std_out), StdStream(CreatePipe, UseHandle),
    createProcess, proc, waitForProcess)
import System.Directory (removeFile)

import qualified Data.ByteString.Lazy as BSL

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE

import Data.Digest.Pure.SHA (sha1, showDigest)

import Text.Pandoc.JSON (Block(CodeBlock, Para), Inline(Image), toJSONFilter)

processBlocks :: Block -> IO Block
processBlocks = \case
    CodeBlock (_ , ["plantuml"], _) content -> plantUMLToImg $ T.pack content
    b -> return b

plantUMLToImg :: Text -> IO Block
plantUMLToImg content = do
    path <- renderImage content
    return $ Para [Image [] (path, "")]

renderImage :: Text -> IO FilePath
renderImage content = do
    c <- withFile path WriteMode $ \hnd -> do
        (Just hIn, _, _, p) <-
            createProcess (proc "plantuml" ["-pipe", "-teps"]) { std_in = CreatePipe
                                                               , std_out = UseHandle hnd
                                                               }
        BSL.hPutStr hIn content'
        hClose hIn

        waitForProcess p

    unless (c == ExitSuccess) $ do
        hPutStrLn stderr $ "plantuml invocation exited with: " ++ show c
        removeFile path

        exitWith c

    return path
  where
    content' = TE.encodeUtf8 content
    uniqueName = showDigest . sha1
    path = uniqueName content' ++ ".eps"

main :: IO ()
main = toJSONFilter processBlocks
