{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : Crypto.Cipher.Spritz
-- Copyright : (C) 2014 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : lens
--
-- The original paper for the Spritz cipher can be found here:
--
-- <https://people.csail.mit.edu/rivest/pubs/RS14.pdf>
--
-- This package provides a Haskell implementation of the pseudocode in the paper
-- listed above. It intends to be a direct implementation of the cipher, so we
-- rely heavily on use of the State monad. We also make heavy use of the lens
-- library's combinators, internally, to ease our use of State.
--
-- Lastly, I must give a shout-out to
-- <https://github.com/therealjampers/spritzjs spritzjs> for the bitmasking
-- parts and for existing so that I had something to test against when I was
-- done.
----------------------------------------------------------------------------

module Crypto.Cipher.Spritz (
  -- * State/Lenses
  SpritzState (..),
  i, j, k, z, w, a, s, n,

  -- * Spritz basic functions
  initializeState,
  absorb,
  absorbByte,
  absorbNibble,
  absorbStop,
  shuffle,
  whip,
  crush,
  squeeze,
  drip,
  update,
  output,

  -- * Helper functions
  low,
  high,
  plusmod,
  submod,
  swap,

  -- * Making use of everything
  hash) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict (State, evalState)
import Control.Monad
import Data.Bits
import qualified Data.Vector as V

data SpritzState = SpritzState
                   { _i :: Int
                   , _j :: Int
                   , _k :: Int
                   , _z :: Int
                   , _w :: Int
                   , _a :: Int
                   , _s :: V.Vector Int
                   , _n :: Int
                   } deriving (Show, Eq, Ord)

makeLenses ''SpritzState

initializeState :: Int -> SpritzState
initializeState n' = SpritzState 0 0 0 0 1 0 (V.fromList [0..n'-1]) n'

absorb :: V.Vector Int -> State SpritzState ()
absorb i' =
  mapM_ (\v -> absorbByte $ i' V.! v) [0..V.length i' - 1]

absorbByte :: Int -> State SpritzState ()
absorbByte b = do
  absorbNibble $ low b
  absorbNibble $ high b

absorbNibble :: Int -> State SpritzState ()
absorbNibble x = do
  a' <- use a
  n' <- use n
  let floored = floor $ (fromIntegral n' :: Double) / 2
  when (a' == floored)
    shuffle
  a'' <- use a
  swap a'' (plusmod floored x n')
  a .= plusmod a'' 1 n'

absorbStop :: State SpritzState ()
absorbStop = do
  a' <- use a
  n' <- use n
  when (a' == floor ((fromIntegral n' :: Double) / 2))
    shuffle
  a'' <- use a
  a .= plusmod a'' 1 n'

shuffle :: State SpritzState ()
shuffle = do
  n' <- use n
  whip (2 * n')
  crush
  whip (2 * n')
  crush
  whip (2 * n')
  a .= 0

whip :: Int -> State SpritzState ()
whip r = do
  replicateM_ r update
  bumpW
  where
    bumpW = do
      w' <- use w
      n' <- use n
      w .= plusmod w' 1 n'
      w'' <- use w
      unless (gcd w'' n' == 1) bumpW

crush :: State SpritzState ()
crush = do
  n' <- use n
  let n'' = floor ((fromIntegral n' :: Double) / 2) - 1
  mapM_ (f n') [0..n'']
  where
    f n' v = do
      s' <- use s
      let idx = n' - 1 - v
      when ((s' V.! v) > (s' V.! idx)) $
        swap v idx

squeeze :: Int -> State SpritzState (V.Vector Int)
squeeze r = do
  a' <- use a
  when (a' > 0)
    shuffle
  V.fromList <$> replicateM r drip

drip :: State SpritzState Int
drip = do
  a' <- use a
  when (a' > 0)
    shuffle
  update
  output

update :: State SpritzState ()
update = do
  w' <- use w
  k' <- use k
  s' <- use s
  j' <- use j
  i' <- use i
  n' <- use n
  i .= plusmod i' w' n'
  i'' <- use i
  j .= plusmod k' (s' V.! plusmod j' (s' V.! i'') n') n'
  j'' <- use j
  k .= plusmod (i'' + k') (s' V.! j'') n'
  swap i'' j''

output :: State SpritzState Int
output = do
  j' <- use j
  s' <- use s
  i' <- use i
  z' <- use z
  k' <- use k
  n' <- use n
  z .= s' V.! plusmod j' (s' V.! plusmod i' (s' V.! plusmod z' k' n') n') n'
  use z

low :: (Bits a, Num a,Show a) => a -> a
low b = b .&. 0xf

high :: (Bits a, Num a,Show a) => a -> a
high b = b `shiftR` 4 .&. 0xf

plusmod :: Integral a => a -> a -> a -> a
plusmod a' b' n' = (a' + b') `mod` n'

submod :: Integral a => a -> a -> a -> a
submod a' b' n' = plusmod n' (a' - b') n'

-- | Swap two elements given indices of S.
swap :: Int -> Int -> State SpritzState ()
swap i' j' = do
  s' <- use s
  let tmpI = s' V.! i'
      tmpJ = s' V.! j'
  s . ix j' .= tmpI
  s . ix i' .= tmpJ

-- | Produces an r-byte hash of the input message.
hash :: V.Vector Int -> Int -> SpritzState -> V.Vector Int
hash m r = evalState $ do
  absorb m
  absorbStop
  absorb (V.fromList [r .&. 0xff])
  squeeze r
