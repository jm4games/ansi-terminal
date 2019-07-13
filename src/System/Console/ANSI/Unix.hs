#include "Common-Safe-Haskell.hs"
{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Unix
  (
-- This file contains code that is common to modules
-- System.Console.ANSI.Unix and System.Console.ANSI.Windows, namely the module
-- exports and the associated Haddock documentation.
#include "Exports-Include.hs"
  ) where

import Control.Exception.Base (bracket)
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hGetEcho,
  hIsTerminalDevice, hIsWritable, hSetBuffering, hSetEcho, stdin,
  stdout)
import Text.ParserCombinators.ReadP (readP_to_S)

import System.Console.ANSI.Codes
import System.Console.ANSI.Types

import qualified Data.ByteString.Builder as BB

-- This file contains code that is common to modules System.Console.ANSI.Unix,
-- System.Console.ANSI.Windows and System.Console.ANSI.Windows.Emulator, such as
-- type signatures and the definition of functions specific to stdout in terms
-- of the corresponding more general functions, inclduding the related Haddock
-- documentation.
#include "Common-Include.hs"
-- This file contains code that is common save that different code is required
-- in the case of the module System.Console.ANSI.Windows.Emulator (see the file
-- Common-Include-Emulator.hs in respect of the latter).
#include "Common-Include-Enabled.hs"

hCursorUp h n = BB.hPutBuilder h $ cursorUpCode n
hCursorDown h n = BB.hPutBuilder h $ cursorDownCode n
hCursorForward h n = BB.hPutBuilder h $ cursorForwardCode n
hCursorBackward h n = BB.hPutBuilder h $ cursorBackwardCode n

hCursorDownLine h n = BB.hPutBuilder h $ cursorDownLineCode n
hCursorUpLine h n = BB.hPutBuilder h $ cursorUpLineCode n

hSetCursorColumn h n = BB.hPutBuilder h $ setCursorColumnCode n
hSetCursorPosition h n m = BB.hPutBuilder h $ setCursorPositionCode n m

hSaveCursor h = BB.hPutBuilder h saveCursorCode
hRestoreCursor h = BB.hPutBuilder h restoreCursorCode
hReportCursorPosition h = BB.hPutBuilder h reportCursorPositionCode

hClearFromCursorToScreenEnd h = BB.hPutBuilder h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h
    = BB.hPutBuilder h clearFromCursorToScreenBeginningCode
hClearScreen h = BB.hPutBuilder h clearScreenCode

hClearFromCursorToLineEnd h = BB.hPutBuilder h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = BB.hPutBuilder h clearFromCursorToLineBeginningCode
hClearLine h = BB.hPutBuilder h clearLineCode

hScrollPageUp h n = BB.hPutBuilder h $ scrollPageUpCode n
hScrollPageDown h n = BB.hPutBuilder h $ scrollPageDownCode n

hSetSGR h sgrs = BB.hPutBuilder h $ setSGRCode sgrs

hHideCursor h = BB.hPutBuilder h hideCursorCode
hShowCursor h = BB.hPutBuilder h showCursorCode

hSetTitle h title = BB.hPutBuilder h $ setTitleCode title

-- hSupportsANSI :: Handle -> IO Bool
-- (See Common-Include.hs for Haddock documentation)
--
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> isNotDumb
 where
  -- cannot use lookupEnv since it only appeared in GHC 7.6
  isNotDumb = (/= Just "dumb") . lookup "TERM" <$> getEnvironment

-- hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)
-- (See Common-Include.hs for Haddock documentation)
hSupportsANSIWithoutEmulation h =
  Just <$> ((&&) <$> hIsWritable h <*> hSupportsANSI h)

-- getReportedCursorPosition :: IO String
-- (See Common-Include.hs for Haddock documentation)
getReportedCursorPosition = bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
  hSetEcho stdin False   -- Turn echo off
  get
 where
  get = do
    c <- getChar
    if c == '\ESC'
      then get' [c]
      else return [c] -- If the first character is not the expected \ESC then
                      -- give up. This provides a modicom of protection against
                      -- unexpected data in the input stream.
  get' s = do
    c <- getChar
    if c /= 'R'
      then get' (c:s) -- Continue building the list, until the expected 'R'
                      -- character is obtained. Build the list in reverse order,
                      -- in order to avoid O(n^2) complexity.
      else return $ reverse (c:s) -- Reverse the order of the built list.

-- getCursorPosition0 :: IO (Maybe (Int, Int))
-- (See Common-Include.hs for Haddock documentation)
getCursorPosition0 = fmap to0base <$> getCursorPosition
 where
  to0base (row, col) = (row - 1, col - 1)
  getCursorPosition = do
    input <- bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
      hSetBuffering stdin NoBuffering -- set no buffering (the contents of the
                                      -- buffer will be discarded, so this needs
                                      -- to be done before the cursor positon is
                                      -- emitted)
      reportCursorPosition
      hFlush stdout -- ensure the report cursor position code is sent to the
                    -- operating system
      getReportedCursorPosition
    case readP_to_S cursorPosition input of
      [] -> return Nothing
      [((row, col),_)] -> return $ Just (row, col)
      (_:_) -> return Nothing
