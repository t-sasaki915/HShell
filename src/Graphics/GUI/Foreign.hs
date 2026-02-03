{-# OPTIONS_GHC -Wno-unsupported-calling-conventions #-}

module Graphics.GUI.Foreign
    ( c_SetClassLongPtr
    , c_GetClassLongPtr
    , c_SetWindowPos
    ) where

import           Data.Int       (Int32)
import           Foreign        (Ptr)
import           Foreign.C      (CIntPtr (..))
import           Graphics.Win32 (BOOL, HWND, UINT)

foreign import stdcall "windows.h SetClassLongPtrW"
  c_SetClassLongPtr :: HWND -> Int32 -> Ptr () -> IO (Ptr ())

foreign import stdcall "windows.h SetWindowPos"
  c_SetWindowPos :: HWND -> HWND -> Int32 -> Int32 -> Int32 -> Int32 -> UINT -> IO BOOL

foreign import ccall "GetClassLongPtrW"
  c_GetClassLongPtr :: HWND -> Int -> IO CIntPtr
