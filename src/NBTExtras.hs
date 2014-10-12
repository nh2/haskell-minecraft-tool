module NBTExtras where

import qualified Data.Array as A
import Data.NBT
import qualified Data.Text as T
import Text.Printf
import Data.Generics.Zipper


-- This file could be home to some of the generic zipper helpers.
-- Get the rightmost (down) element of a zipper is the relevant data:
--   ByteTag -> Int8
--   ShortTag -> Int16
--   IntTag (Maybe String) Int32
--   LongTag -> Int64
--   FloatTag -> Float
--   DoubleTag -> Double
--   ByteArrayTag -> ByteString
--   StringTag -> String
--   ListTag -> [NBT]
--   CompoundTag -> [NBT]
getData nbt = down nbt >>= getHole

-- Some standard extractors
getName :: NBT -> String
getName (NBT name _) = T.unpack name

getInt (NBT _ (IntTag n)) = fromIntegral n
getInt t = error $ printf "getInt: %s is not an IntTag." (show t)

getList (NBT _ (ListTag _ arr)) = A.elems arr

compoundContents (NBT _ (CompoundTag contents)) = contents
compoundContents _ = error "Not CompoundTag"
