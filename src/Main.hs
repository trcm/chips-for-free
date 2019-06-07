{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified Control.Applicative           as A
import qualified Data.ByteString               as BS
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Chip
import           Data.Vector
import           GHC.Word                       ( Word8
                                                , Word16
                                                )
import Text.Printf
import Data.Bits
import           Data.Function

type Rom = BS.ByteString

data Cpu = Cpu { _pc :: Int }

data EmuState = Running | Stopped | Finished

data Emu m a where
  LoadRom :: FilePath -> Emu m Rom
  ReadInstruction :: Int -> Rom -> Emu m Word16

makeSem ''Emu

data CpuEff m a where
  DecodeOpcode :: Word8 -> CpuEff m a
  -- Process opcode, return next modified state
  ProcessInstruction :: Rom -> Cpu -> CpuEff m Cpu

makeSem ''CpuEff

-- Input will be the filepath of the rom, output will be the buffer printed to screen
runEmu :: (Member (Input FilePath) r, Member (Output Word16) r, Member Emu r) => Sem r ()
runEmu = do
  fp <- input @FilePath 
  rom <- loadRom fp
  let initialCpu = Cpu 0
  is <- readInstruction 0 rom
  output is

romProvider :: Member (Lift IO) r => Sem (Emu ': r) a -> Sem r a
romProvider = interpret $ \case
  LoadRom fp -> sendM $ BS.readFile fp
  ReadInstruction pc rom -> let upper = BS.index rom pc
                                lower = BS.index rom (pc + 1)
                            in pure $ mkWord16 upper lower

mkWord16 :: Word8 -> Word8 -> Word16
mkWord16 upper lower = 
  let lo = 0x00FF .&. fromIntegral lower :: Word16
      hi = shift (fromIntegral upper :: Word16) 8
  in  hi .|. lo

runConsoleOutput :: Member (Lift IO) r => Sem (Output Word16 ': r) a -> Sem r a
runConsoleOutput = interpret $ \case
  Output a -> sendM . putStrLn $ printInHex a

printInHex :: Word16 -> String
printInHex a' = printf "0x%02x" a'

main :: IO ()
main = do
  runEmu
    & runConstInput "./roms/PONG"
    & romProvider
    & runConsoleOutput
    & runM

