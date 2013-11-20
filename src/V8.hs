{-# LANGUAGE ForeignFunctionInterface #-}

module Main
    ( 
      main
    , hello
    )
  where

import Foreign.C.Types
import Foreign.C.String


foreign import ccall "hello"
  c_hello :: IO CString


hello :: IO ()
hello = do
    cstr <- c_hello
    str <- peekCString cstr
    putStrLn str
    return ()


main :: IO ()
main = hello
