{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
module Generics where

import Data.Array
import qualified Data.Array.Unboxed as U
import Data.Serialize
import Data.Data
import Data.Derive.Data
import Data.Derive.Typeable
import Data.DeriveTH
import Data.Generics
import Data.Generics.Zipper
import Data.Int
import Data.Monoid
import Data.NBT
import Data.Typeable
import qualified Data.ByteString.Lazy as B

import Block
import Coords
import Chunk
import Types
import Utils
import Control.Applicative

{- Generics -}
-- Make automatic instance derivations for SYB, SYZ.
-- $(derive makeData ''Array)
-- $(derive makeData ''NbtContents)
-- $(derive makeTypeable ''NbtContents)
-- $(derive makeData ''NBT)
-- deriving instance Data (Array Int32 NbtContents)
-- deriving instance Data (Array Int32 Int32)

-- typeArrayInt32Int32 :: DataType
-- typeArrayInt32Int32 = mkIntType "UArray Int32 Int32"

-- instance Data (UArray Int32 Int32) where
--   toConstr x = mkIntegralConstr typeArrayInt32Int32 x
--   gunfold _ z c = case constrRep c of
--                     _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
--                                  ++ " is not of type UArray Int32 Int32."
--   dataTypeOf _ = typeArrayInt32Int32

-- typeArrayInt32Int8 :: DataType
-- typeArrayInt32Int8 = mkIntType "UArray Int32 Int8"

-- instance Data (UArray Int32 Int8) where
--   toConstr x = mkIntegralConstr typeArrayInt32Int8 x
--   gunfold _ z c = case constrRep c of
--                     _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
--                                  ++ " is not of type UArray Int32 Int8."
--   dataTypeOf _ = typeArrayInt32Int8

instance (Typeable a, Data a, Data b, U.Ix a, U.IArray U.UArray b) => Data (U.UArray a b)
 where
  gfoldl f z a = z (U.listArray (U.bounds a)) `f` (U.elems a)
  toConstr _   = error "Data.Data.toConstr(UArray)"
  gunfold _ _  = error "Data.Data.gunfold(UArray)"
  dataTypeOf _ = mkNoRepType "Data.UArray.UArray"
  dataCast2 x  = gcast2 x

deriving instance Data NbtContents
deriving instance Typeable NbtContents
deriving instance Data NBT
deriving instance Typeable NBT
deriving instance Data TagType
deriving instance Typeable TagType

-- value = undefined
-- let g1 = toZipper nbt1 in
-- let Just g2 = down g1 in getHole g2
--        let Just g3 = getHole g2 :: Maybe [NBT]

-- We would like to replace a part of the NBT: strictly speaking, the
-- zeverywhere would help me navigate to the right NBT
--
-- 1) Find the Blocks array and update it.
-- 2) Find the Data array and update it.
--
-- For some reason, the series of updates couldn't be performed all at the same
-- time. I'm not sure why...
--
-- We will just leave all the timestamps the same as before, for simplicity.
-- We accept a number of transformations that would modify the NBT in many
-- ways...
-- TODO move foldr1 (.) fs into its own function 'compose'
updateChunk :: [NBT -> NBT] -> Chunk -> Chunk
updateChunk [] chunk = chunk
updateChunk (f:fs) chunk = updateChunk fs (everywhere (mkT f) chunk) -- . vtrace "Updating NBT: "

-- Transition function that applies to an NBT I'm interested in.
-- These are kind of expensive :/
-- setBlockIds :: B.ByteString -> NBT -> NBT
-- setBlockIds bs (ByteArrayTag (Just "Blocks") len _) =
--   ByteArrayTag (Just "Blocks") (fromIntegral $ B.length bs) bs
-- setBlockIds _ x = x

-- setBlockData :: B.ByteString -> NBT -> NBT
-- setBlockData bs (ByteArrayTag (Just "Data") len _) =
--   ByteArrayTag (Just "Data") (fromIntegral $ B.length bs) bs
-- setBlockData _ x = x

-- -- BuildBlock
blockIdUpdates :: [(LocalCoords, BlockId)] ->  NBT -> NBT
blockIdUpdates updates (NBT "Blocks" (ByteArrayTag arr)) =
  NBT "Blocks" $ ByteArrayTag (fromCoordArr (toCoordArr arr // updates))
blockIdUpdates _ nbt = nbt

blockDataUpdates :: [(LocalCoords, BlockDatum)] ->  NBT -> NBT
blockDataUpdates updates (NBT "Data" (ByteArrayTag arr)) =
  NBT "Data" $ ByteArrayTag (fromCoordArr (toCoordArr arr // updates))
blockDataUpdates _ nbt = nbt

toCoordArr :: U.UArray Int32 Int8 -> Array CellCoords BlockDatum
toCoordArr a = fmap fromIntegral $ ixmap (both toCellCoord $ U.bounds a) fromCellCoord (fromUArray a)

fromCoordArr :: Array CellCoords BlockDatum -> U.UArray Int32 Int8
fromCoordArr a = U.amap fromIntegral . toUArray $ ixmap (both fromCellCoord $ bounds a) toCellCoord a
both f (a,b) = (f a, f b)

fromUArray a = listArray (U.bounds a) (U.elems a)
toUArray a = U.listArray (bounds a) (elems a)

-- If the hole matches then return the z.
-- 'plz' means 'possibleListOfZippers'. This is a [NBT] arising from the last
-- part of either the ListTag or the CompoundTag.
moveToTag :: String -> Zipper NBT -> Maybe (Zipper NBT)
moveToTag name z = do
  z2 <- down' z
  -- This condition fails when no name available, Nothing name, or non-matching name
  if getHole z2 == Just (Just name)
    then Just z
    else do
      plz <- down z
      _ <- getHole plz :: Maybe [NBT] -- ensure this cast to [NBT] is valid
      moveToTagList name plz

-- Continue the threading through multiple items in a list.
-- IF we're looking at a cons, we ensure that the zipper is executed
-- for every one in the list.
-- This is the zipper analogous to 'find'. Look at head by navigation, if , and
moveToTagList :: String -> Zipper NBT -> Maybe (Zipper NBT)
moveToTagList name z = do
  left <- down' z -- Left child; the 'a' of (a:as)
  right <- down z  -- Right child; the 'as' of (a:as)
  getFirst . mconcat $
      map First [moveToTag name left, moveToTagList name right]

