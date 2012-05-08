-- | OpenGL support for Data.Bitmap

{- Copied from the bitmap-opengl package because maintainer is
   unresponsive about bug-fixes/uploads -}
{- Copyright: (c) 2009 Balazs Komuves -}

{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.DrawingCombinators.Bitmap
  ( makeSimpleBitmapTexture
  , makeTextureFromBitmap
  , texImageFromBitmap
  ) where

--------------------------------------------------------------------------------

import Data.Bitmap

import Graphics.Rendering.OpenGL

--------------------------------------------------------------------------------

-- OpenGL data type
dataType :: PixelComponent t => t -> DataType
dataType t = case pixelComponentType t of
  PctWord8  -> UnsignedByte
  PctWord16 -> UnsignedShort
  PctWord32 -> UnsignedInt
  PctFloat  -> Float

--------------------------------------------------------------------------------

-- | This function guesses the pixel format from the number of channels:
-- 
-- * 1 ~> Alpha
--
-- * 2 ~> Luminance, Alpha
--
-- * 3 ~> RGB
--
-- * 4 ~> RGBA
--
-- For more control, use 'makeTextureFromBitmap'.
makeSimpleBitmapTexture :: forall t. PixelComponent t => Bitmap t -> IO TextureObject
makeSimpleBitmapTexture bm = do
  let (pf,pif) = case pixelComponentType (undefined::t) of 
        PctWord8 -> case bitmapNChannels bm of
          1 -> (Alpha, Alpha8)
          2 -> (LuminanceAlpha, Luminance8Alpha8)
          3 -> (RGB, RGB8)
          4 -> (RGBA, RGBA8)  
          n -> error $ "Invalid bitmap channel count: " ++ show n
        _ -> case bitmapNChannels bm of
          1 -> (Alpha, Alpha')
          2 -> (LuminanceAlpha, LuminanceAlpha')
          3 -> (RGB, RGB')
          4 -> (RGBA, RGBA')  
          n -> error $ "Invalid bitmap channel count: " ++ show n
  makeTextureFromBitmap bm Nothing 0 pf pif 0 
  
-- | Creates a new OpenGL texture from a bitmap
makeTextureFromBitmap 
  :: PixelComponent t 
  => Bitmap t -> Maybe CubeMapTarget -> Level -> PixelFormat -> PixelInternalFormat -> Border -> IO TextureObject
makeTextureFromBitmap bm cubemap level pf pif border = do
  old_binding <- get (textureBinding Texture2D)
  [tex] <- genObjectNames 1 
  textureBinding Texture2D $= Just tex 
  textureFilter Texture2D $= ((Linear',Nothing),Linear')
  texImageFromBitmap bm cubemap level pf pif border   
  textureBinding Texture2D $= old_binding
  return tex

texImageFromBitmap
  :: forall t. PixelComponent t 
  => Bitmap t -> Maybe CubeMapTarget -> Level -> PixelFormat -> PixelInternalFormat -> Border -> IO ()
texImageFromBitmap bm cubemap level pf pif border = do
  withBitmap bm $ \(width,height) _nchn _pad ptr -> do
--    old_rowlength <- get (rowLength Unpack)
    old_alignment <- get (rowAlignment Unpack)
    let pdata = PixelData pf (dataType (undefined::t)) ptr  
        size = TextureSize2D (fromIntegral width) (fromIntegral height) 
--    rowLength Unpack $= fromIntegral (bitmapPaddedRowSizeInBytes bm)
    rowAlignment Unpack $= fromIntegral (bitmapRowAlignment bm)
    texImage2D cubemap NoProxy level pif size border pdata
--    rowLength Unpack $= old_rowlength 
    rowAlignment Unpack $= old_alignment
  
--------------------------------------------------------------------------------
