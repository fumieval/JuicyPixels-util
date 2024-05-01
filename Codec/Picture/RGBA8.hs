{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, MagicHash, CPP #-}
module Codec.Picture.RGBA8 where

import Codec.Picture
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Marshal.Utils
import Control.Monad
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe
import Data.Bits
import GHC.Ptr
import GHC.Base
#if MIN_VERSION_base(4,15,0)
import GHC.Integer
#else
import GHC.Num
#endif

class ToPixelRGBA8 a where
    toRGBA8 :: a -> PixelRGBA8
    
instance ToPixelRGBA8 Pixel8 where
    toRGBA8 b = PixelRGBA8 b b b 255

instance ToPixelRGBA8 PixelYA8 where
    toRGBA8 (PixelYA8 l a) = PixelRGBA8 l l l a

instance ToPixelRGBA8 PixelRGB8 where
    toRGBA8 (PixelRGB8 r g b) = PixelRGBA8 r g b 255

instance ToPixelRGBA8 PixelRGBA8 where
    toRGBA8 = id

fromColorAndOpacity :: PixelRGB8 -> Image Pixel8 -> Image PixelRGBA8
fromColorAndOpacity (PixelRGB8 r g b) (Image w h vec) = Image w h $ V.generate (w * h * 4) pix where
    pix i = if testBit i 0
        then if testBit i 1
            then vec V.! unsafeShiftR i 2
            else b
        else if testBit i 1
            then g
            else r
    {-# INLINE pix #-}

-- | Note: Accessing out of the image causes a segfault.
trimImage :: Image PixelRGBA8
    -> (Int, Int) -- ^ width, height
    -> (Int, Int) -- ^ the left corner point
    -> Image PixelRGBA8
trimImage (Image w _ vec) (w', h') (x0, y0) = unsafePerformIO $ V.unsafeWith vec $ \ptr -> do
    mv <- MV.unsafeNew $ w' * h' * 4
    MV.unsafeWith mv $ \dst -> forM_ [0..h'-1] $ \y -> 
        copyBytes (plusPtr dst $ y * w' * 4) (plusPtr ptr $ (*4) $ (y + y0) * w + x0) (4 * w')
    Image w' h' `fmap` V.unsafeFreeze mv

-- | Note: Accessing out of the image causes a segfault.
patchImage :: Image PixelRGBA8 -> (Int, Int) -> Image PixelRGBA8 -> Image PixelRGBA8
patchImage (Image w h target) (x0, y0) (Image w' h' vec) = unsafePerformIO $ V.unsafeWith vec $ \ptr -> do
    mv <- V.thaw target
    MV.unsafeWith mv $ \dst -> forM_ [0..h'-1] $ \y ->
        copyBytes (plusPtr dst $ (*4) $ (y + y0) * w + x0) (plusPtr ptr $ (*4) $ y * w') (4 * w')
    Image w h `fmap` V.unsafeFreeze mv

flipVertically :: Image PixelRGBA8 -> Image PixelRGBA8
flipVertically (Image w h v) = unsafePerformIO $ V.unsafeWith v $ \ptr -> do
    mv <- MV.unsafeNew $ w * h * 4
    MV.unsafeWith mv $ \dst -> forM_ [0..h-1] $ \y -> copyBytes (plusPtr dst $ y * w * 4) (plusPtr ptr $ (h - y - 1) * w * 4) (4 * w)
    Image w h `fmap` V.unsafeFreeze mv

fromDynamicImage :: DynamicImage -> Image PixelRGBA8
fromDynamicImage (ImageY8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageYA8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGB8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGBA8 img) = img
fromDynamicImage _ = error "Unsupported format"

readImageRGBA8 :: FilePath -> IO (Image PixelRGBA8)
readImageRGBA8 path = readImage path >>= either fail (return . fromDynamicImage)

addrImage :: Image PixelRGBA8 -> IO Int
addrImage (Image _ _ v) = let (fptr, _) = V.unsafeToForeignPtr0 v
                            in withForeignPtr fptr $ \(Ptr a) -> return $ fromEnum $ wordToInteger (int2Word# (addr2Int# a))