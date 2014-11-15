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
  -- ** Encyrption
  encrypt,
  decrypt,
  keySetup,

  -- ** Hashing
  hash,

  -- ** Message Authentication Code (MAC)
  mac,
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict (State, evalState, put)
import Control.Monad
import Data.Bits
import qualified Data.Vector as V

-- | Register values and 's'. As a __difference to the paper__, we also include
-- 'n' in the state, for easy access to it within the various functions.
--
-- See §3.1 "State".
data SpritzState = SpritzState
                   { _i :: Int
                   -- ^ Register: Always incremented by @'w' `'mod'` 'n'@
                   -- whenever changed.
                   , _j :: Int
                   -- ^ Register
                   , _k :: Int
                   -- ^ Register
                   , _z :: Int
                   -- ^ Register: Last value produced by 'output'
                   , _w :: Int
                   -- ^ Register: Modified when 'whip' is called. Always
                   -- relatively prime to 'n'.
                   , _a :: Int
                   -- ^ Register: Number of nibbles absorbed since start of last
                   -- 'shuffle' of 's'.
                   , _s :: V.Vector Int
                   -- ^ Length 'n'. Permutation of Z_n.
                   , _n :: Int          -- ^ All values in Spritz are modulo 'n'.
                   } deriving (Show, Eq, Ord)

makeLenses ''SpritzState

-- | Returns the standard initial state. See §3.2 "InitializeState".
initializeState :: Int         -- ^ The 'n' value to use throughout.
                -> SpritzState -- ^ The initial state.
initializeState n' = SpritzState 0 0 0 0 1 0 (V.fromList [0..n'-1]) n'

-- | Takes a variable-lenght input and updates the state based on it. Spritz
-- absorbs input in blocks of @'floor' ('n' / 2)@ nibbles each (low-order
-- nibble of each byte first). After each block is absorbed, we call 'shuffle'.
--
-- Satisfies the following law:
--
--    @'absorb' x1 >> 'absorb' x2 = 'absorb (x1 ++ x2)'@
--
-- See §3.2 "Absorb".
absorb :: V.Vector Int -> State SpritzState ()
absorb i' =
  mapM_ (\v -> absorbByte $ i' V.! v) [0..V.length i' - 1]

-- | Splits the given input byte into two nibbles and updates state based on
-- each nibble, low-order nibble first. See §3.2 "AbsorbByte".
absorbByte :: Int -> State SpritzState ()
absorbByte b = do
  absorbNibble $ low b
  absorbNibble $ high b

-- | TODO: Write documentation. See §3.2 "AbsorbNibble".
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

-- | Equivalent to absorbing a special "stop" symbol outside of the oridnary
-- input alphabet. The intent is to provide a clean way to separate different
-- inputs being absorbed. See §2.1.
absorbStop :: State SpritzState ()
absorbStop = do
  a' <- use a
  n' <- use n
  when (a' == floor ((fromIntegral n' :: Double) / 2))
    shuffle
  a'' <- use a
  a .= plusmod a'' 1 n'

-- | 'whip's, 'crush'es, 'whip's, 'crush'es, and finally 'whip's again.
-- According to the paper, each 'whip' randomizes the state. Calling 'crush'
-- between each 'whip' causes the effects of 'crush' to be not easily determined
-- by manipulating the input.  See §3.2 "Shuffle".
shuffle :: State SpritzState ()
shuffle = do
  n' <- use n
  whip (2 * n')
  crush
  whip (2 * n')
  crush
  whip (2 * n')
  a .= 0

-- | Calls 'update' @r@ times. Also updates 'w' to the next largest value that
-- is relatively prime to 'n'.
whip :: Int -- ^ @r@. The number of times to call 'update'.
     -> State SpritzState ()
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

-- | A utility function that adds the first parameter to the second then returns
-- that modulo the third parameter ('n'). This is used throughout Spritz in
-- place of a more traditional 'xor' approach so that 'n' can be any value and
-- is not limited to being a power of 2.
plusmod :: Integral a => a -> a -> a -> a
plusmod a' b' n' = (a' + b') `mod` n'


-- | See 'plusmod'. This is very similar except it subtracts the first two
-- arguments instead of adding them.
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

-----------------------------------------------------------------------------
-- Encryption
-----------------------------------------------------------------------------
-- | Adds-modulo-N ('plusmod') each byte of the message with the corresponding
-- byte of the output of 'squeeze' yielding an ecrypted ciphertext. See §2.2.
encrypt :: V.Vector Int -- ^ The key.
        -> V.Vector Int -- ^ The decrypted message.
        -> SpritzState  -- ^ Starting state.
        -> V.Vector Int
encrypt k' m' = evalState $ do
  n' <- use n
  -- We do this instead of calling keySetup so the user can pass a state.
  absorb k'
  m'' <- squeeze (V.length m')
  return $ V.map (uncurry (\x y -> plusmod x y n')) (V.zip m' m'')

-- | Decrypts a message encrypted with 'encrypt'. Identical to 'encrypt' except
-- uses 'submod' instead. See §2.2.
decrypt :: V.Vector Int -- ^ The key.
        -> V.Vector Int -- ^ The encrypted message.
        -> SpritzState  -- ^ Starting state.
        -> V.Vector Int
decrypt k' c' = evalState $ do
  n' <- use n
  -- We do this instead of calling keySetup so the user can pass a state.
  absorb k'
  m'' <- squeeze (V.length c')
  return $ V.map (uncurry (\x y -> submod x y n')) (V.zip c' m'')

-- | Used in the paper at the top of 'encrypt'* and 'decrypt', but not used by
-- default in this library. Still, we provide it in case it's needed.
--
-- @keySetup n' k' = put (initializeState n') >> absorb k'@
keySetup :: Int -- ^ Our N value. 256 in the paper.
         -> V.Vector Int -- ^ The key.
         -> State SpritzState ()
keySetup n' k' =
  put (initializeState n') >> absorb k'

-----------------------------------------------------------------------------
-- Hashing
-----------------------------------------------------------------------------
-- | Produces an @r@-byte hash of the input message.
--
-- 'hash' absorbs the input message, calls 'absorbStop' to signal the end of the
-- input message, then absorbs the desired hash length (@r@).
--
-- The given @r@ is absorbed for functional separation.
--
-- See §2.3.
hash :: V.Vector Int -- ^ The message.
     -> Int          -- ^ r (number of bytes).
     -> SpritzState  -- ^ Initial state.
     -> V.Vector Int
hash m r = evalState $ do
  absorb m
  absorbStop
  absorb (V.fromList [r .&. 0xff])
  squeeze r

-----------------------------------------------------------------------------
-- Message Authentication Code (MAC)
-----------------------------------------------------------------------------
-- | Message authentication code. See §2.4.
mac :: V.Vector Int -- ^ The key.
    -> V.Vector Int -- ^ The message.
    -> Int -- ^ r
    -> SpritzState
    -> V.Vector Int
mac k' m' r' = evalState $ do
  absorb k'
  absorbStop
  absorb m'
  absorbStop
  absorb (V.fromList [r' .&. 0xff])
  squeeze r'
