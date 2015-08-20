{-# LANGUAGE ScopedTypeVariables #-}

module Data.ByteString.Storable (get, getHead) where

import qualified Data.ByteString as B
import Data.ByteString.Internal
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import qualified Data.Traversable as DT
import System.IO.Unsafe

-- | Get the value at offset n in the ByteString. Returns Nothing if the 
--   ByteString is too short to contain the value or it isn't aligned. 
get :: Storable a => ByteString -> Int -> Maybe a
get (PS fp o l) n = unsafePerformIO $  
                        withForeignPtr fp $ 
                            \p -> DT.sequence $ readPtr p (o + n) l

-- | A convenience function to get the value at the head of the bytestring.
getHead :: Storable a => ByteString -> Maybe a
getHead b = get b 0 

readPtr :: forall a . Storable a => Ptr Word8 -> Int -> Int -> Maybe (IO a)
readPtr p o l = let
                    ap = castPtr p
                    last = o + sizeOf (undefined :: a)
                    isAligned = (fromIntegral (ptrToIntPtr ap) + o) `mod`
                            (alignment (undefined :: a)) == 0
               in
                    if (last <= l && isAligned) then Just (peekByteOff ap o) else Nothing
