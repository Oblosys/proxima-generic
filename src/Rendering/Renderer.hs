{-# LANGUAGE CPP #-} 
-- CPP is enabled only for this module, since it slows the build process down quite a bit
module Rendering.Renderer (render, renderFocus, mkPopupMenuXY, computeUpdatedRegions) where

import Common.CommonTypes 
import Common.CommonUtils

import Rendering.RenLayerTypes
import Rendering.RenLayerUtils
import Proxima.Wrap

#ifdef SERVER
import Rendering.RendererServer
#else
import Rendering.RendererGtk
#endif


computeUpdatedRegions oldUpdRegions scale focus diffTree oldArrangement arrangement =
  -- showDebug' Err ("updated regions for\n" ++ show oldArrangement ++"\n\n\n" ++ show arrangement ++ "\n\n\n\n" ) $
  let (oldW,oldH) = (widthA oldArrangement, heightA oldArrangement)
      (newW,newH) = (widthA arrangement, heightA arrangement)
  in if oldW>newW || oldH > newH     -- if arr got smaller, repaint whole thing for now
     then [((0, 0),(max oldW newW, max oldH newH))]
     else updatedRectArr diffTree arrangement  
