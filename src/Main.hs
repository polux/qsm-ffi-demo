-- Copyright (c) 2017-2019 Stevan Andjelkovic, Daniel
-- Gustafsson, Jacob Stanley, Xia Li-yao, Robert Danitz,
-- Thomas Winant, Edsko de Vries, Momoko Hattori, Kostas
-- Dermentzis, Adam Boniecki
--
-- Copyright (c) 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Applicative ()
import Control.Monad (guard)
import Data.Function ()
import Data.Functor.Classes (Eq1)
import Data.IORef ()
import Data.Maybe (isJust)
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic, Generic1)
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO)
import Foreign.C (CBool(..), CInt(..))
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr )
import Test.StateMachine
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Prelude hiding (elem)

------------------------------------------------------------------------

type CBuffer = ()

foreign import ccall "buffer.h new_buffer"
  new_buffer :: CInt -> IO (Ptr CBuffer)
foreign import ccall "buffer.h put_buffer"
  put_buffer :: CInt -> Ptr CBuffer -> IO ()
foreign import ccall "buffer.h get_buffer"
  get_buffer :: Ptr CBuffer -> IO CInt
foreign import ccall "buffer.h len_buffer"
  len_buffer :: Ptr CBuffer -> IO CInt
foreign import ccall "buffer.h same_elements"
  same_elements :: Ptr CBuffer -> Ptr CBuffer -> CBool
foreign import ccall "buffer.h &delete_buffer"
  delete_buffer :: FunPtr (Ptr CBuffer -> IO ())

newtype Buffer = Buffer (ForeignPtr CBuffer)

instance Eq Buffer where
  (Buffer b1) == (Buffer b2) = b1 == b2

newBuffer :: Int -> IO Buffer
newBuffer n = do
  ptr <- new_buffer (fromIntegral n)
  fptr <- newForeignPtr delete_buffer ptr
  return (Buffer fptr)

putBuffer :: Int -> Buffer -> IO ()
putBuffer x (Buffer fptr) = withForeignPtr fptr (put_buffer (fromIntegral x))

getBuffer :: Buffer -> IO Int
getBuffer (Buffer fptr) = fromIntegral <$> withForeignPtr fptr get_buffer

lenBuffer :: Buffer -> IO Int
lenBuffer (Buffer fptr) = fromIntegral <$> withForeignPtr fptr len_buffer

sameElements :: Buffer -> Buffer -> IO Bool
sameElements (Buffer fptr1) (Buffer fptr2) =
  withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
      return (same_elements ptr1 ptr2 /= 0)

------------------------------------------------------------------------

-- | Buffer actions.
data Action (r :: * -> *)
    -- | Create a new buffer of bounded capacity.
  = New Int

    -- | Put an element at the top of the buffer.
  | Put Int (Reference (Opaque Buffer) r)

    -- | Get an element out of the bottom of the buffer.
  | Get (Reference (Opaque Buffer) r)

    -- | Get the number of elements in the buffer.
  | Len (Reference (Opaque Buffer) r)
  | SameElem (Reference (Opaque Buffer) r) (Reference (Opaque Buffer) r)
  deriving (Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

data Response (r :: * -> *)
  = NewR (Reference (Opaque Buffer) r)
  | PutR
  | GetR Int
  | LenR Int
  | SameR Bool
  deriving (Show, Generic1, Rank2.Foldable, CommandNames)

------------------------------------------------------------------------

-- | A simple, persistent, inefficient buffer.
--
-- The top of the buffer is the head of the list, the bottom is the last
-- element.
data SpecBuffer = SpecBuffer
  { specSize     :: Int    -- ^ Maximum number of elements
  , specContents :: [Int]  -- ^ Contents of the buffer
  }
  deriving (Generic, Show, ToExpr)

emptySpecBuffer :: Int -> SpecBuffer
emptySpecBuffer n = SpecBuffer n []

insertSpecBuffer :: Int -> SpecBuffer -> SpecBuffer
insertSpecBuffer x (SpecBuffer n xs) = SpecBuffer n (x : xs)

removeSpecBuffer :: SpecBuffer -> (Int, SpecBuffer)
removeSpecBuffer (SpecBuffer n xs) = (last xs, SpecBuffer n (init xs))

sameElementsSpecBuffer :: SpecBuffer -> SpecBuffer -> Bool
sameElementsSpecBuffer (SpecBuffer n1 xs1) (SpecBuffer n2 xs2) = xs1 == xs2

------------------------------------------------------------------------

-- | The model is a map from buffer references to their values.
newtype Model r = Model [(Reference (Opaque Buffer) r, SpecBuffer)]
  deriving (Generic, Show)

deriving instance ToExpr (Model Concrete)

-- | Initially, there are no references to buffers.
initModel :: Model v
initModel = Model []

precondition :: Model Symbolic -> Action Symbolic -> Logic
precondition _         (New n) = n .> 0
precondition (Model m) (Put _ buffer) = Boolean $ isJust $ do
  specBuffer <- lookup buffer m
  guard $ length (specContents specBuffer) < specSize specBuffer
precondition (Model m) (Get buffer) = Boolean $ isJust $ do
  specBuffer <- lookup buffer m
  guard $ not (null (specContents specBuffer))
precondition (Model m) (Len buffer) = buffer `elem` map fst m
precondition (Model m) (SameElem buffer1 buffer2) =
  buffer1 `elem` buffers .&& buffer2 `elem` buffers
 where
  buffers = map fst m

transition :: Eq1 r => Model r -> Action r -> Response r -> Model r
transition (Model m) (New n)        (NewR ref) =
  Model ((ref, emptySpecBuffer n) : m)
transition (Model m) (Put x buffer) _          =
  case lookup buffer m of
    Just old -> Model (update buffer (insertSpecBuffer x old) m)
    Nothing  -> error "transition: put"
transition (Model m) (Get buffer) _ =
  case lookup buffer m of
    Just old ->
      let (_, new) = removeSpecBuffer old in
      Model (update buffer new m)
    Nothing  -> error "transition: get"
transition m    _ _ = m

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update ref i m = (ref, i) : filter ((/= ref) . fst) m

postcondition :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postcondition _         (New _)      _        = Top
postcondition _         (Put _ _)    _        = Top
postcondition (Model m) (Get buffer) (GetR y) = case lookup buffer m of
  Nothing         -> Bot
  Just specBuffer ->
    let (y', _) = removeSpecBuffer specBuffer
    in y .== y'
postcondition (Model m) (Len buffer) (LenR k) = case lookup buffer m of
  Nothing         -> Bot
  Just specBuffer -> k .== length (specContents specBuffer)
postcondition (Model m) (SameElem buffer1 buffer2) (SameR b) =
  case lookup buffer1 m of
    Nothing -> Bot
    Just specBuffer1 ->
      case lookup buffer2 m of
        Nothing -> Bot
        Just specBuffer2 ->
          b .== sameElementsSpecBuffer specBuffer1 specBuffer2
postcondition _         _            _        = error "postcondition"

------------------------------------------------------------------------

genNew :: Gen (Action Symbolic)
genNew = do
  Positive n <- arbitrary
  return (New n)

generator :: Model Symbolic -> Gen (Action Symbolic)
generator (Model m) | null m = genNew
generator (Model m)          = frequency $
  [ (1, genNew)
  , (4, Put <$> arbitrary <*> (fst <$> elements m))
  , (4, Get <$> (fst <$> elements m))
  , (4, Len <$> (fst <$> elements m))
  , (4, SameElem <$> (fst <$> elements m) <*> (fst <$> elements m))]

shrinker :: Action Symbolic -> [Action Symbolic]
shrinker (New n)        = [ New n'        | n' <- shrink n ]
shrinker (Put x buffer) = [ Put x' buffer | x' <- shrink x ]
shrinker _              = []

------------------------------------------------------------------------

semantics :: Action Concrete -> IO (Response Concrete)
semantics (New n)        = NewR . reference . Opaque <$> newBuffer n
semantics (Put x buffer) = PutR <$  putBuffer x (opaque buffer)
semantics (Get buffer)   = GetR <$> getBuffer (opaque buffer)
semantics (Len buffer)   = LenR <$> lenBuffer (opaque buffer)
semantics (SameElem buffer1 buffer2) = SameR <$> sameElements (opaque buffer1) (opaque buffer2)

mock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
mock _         (New _)      = NewR <$> genSym
mock _         (Put _ _)    = pure PutR
mock (Model m) (Get buffer) = case lookup buffer m of
  Nothing   -> error "mock: get"
  Just spec -> case specContents spec of
    []      -> error "mock: get 2"
    (i : _) -> pure (GetR i)
mock (Model m) (Len buffer) = case lookup buffer m of
  Nothing   -> error "mock: len"
  Just spec -> pure (LenR (specSize spec))
mock (Model m) (SameElem buffer1 buffer2) = pure (SameR True)

------------------------------------------------------------------------

sm :: StateMachine Model Action IO Response
sm = StateMachine initModel
                  transition
                  precondition
                  postcondition
                  Nothing
                  (Just . generator)
                  Nothing
                  (const shrinker)
                  semantics
                  mock

prop_circularBuffer :: Property
prop_circularBuffer =
  forAllCommands sm (Just 1000) $ \cmds -> monadicIO $ do
    (hist, _, res) <- runCommands sm cmds
    prettyCommands sm hist $
      checkCommandNames cmds (res === Ok)

prop_circularBuffer_parallel :: Property
prop_circularBuffer_parallel =
  forAllParallelCommands sm $ \cmds -> monadicIO $ do
    runParallelCommands sm cmds >>= prettyParallelCommands cmds


main = do
  quickCheck (withMaxSuccess 1000 prop_circularBuffer)
  --quickCheck (withMaxSuccess 1000 prop_circularBuffer_parallel)
