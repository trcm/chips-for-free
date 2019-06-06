{-# LANGUAGE TemplateHaskell #-}
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
import           Data.Function

type Rom = BS.ByteString

data Cpu = Cpu { _pc :: Int }

data EmuState = Running | Stopped | Finished

data Emu m a where
  ReadInstruction :: Int -> Rom -> Emu m Word8

makeSem ''Emu

data CpuEff m a where
  DecodeOpcode :: Word8 -> CpuEff m a
  ProcessInstruction :: a -> CpuEff m a

makeSem ''CpuEff

loadRom :: IO BS.ByteString
loadRom = BS.readFile "./roms/PONG"

startEmu :: (Member (Input Rom) r, Member (Input Cpu) r, Member Emu r) => Sem r ()
startEmu = do
  rom <- input @Rom
  cpu <- input @Cpu
  readInstruction (_pc cpu) rom
  undefined

runEmu :: Sem (Emu ': r ) a -> Sem r a
runEmu = interpret $ \case
  ReadInstruction pc rom -> do
    pure $ BS.index rom pc

-- echo forever
main :: IO ()
main = do
  rom <- loadRom
  startEmu & runConstInput @Rom rom & runConstInput @Cpu (Cpu 0) & runEmu & runM


