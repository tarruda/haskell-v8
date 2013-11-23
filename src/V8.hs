{-# LANGUAGE ForeignFunctionInterface #-}

module V8
    ( 
        newContext
    ,   evalInContext
    )
  where


import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String


type V8Context = Ptr ()
type V8Value = Ptr ()


newContext :: IO V8Context
newContext = c_new_context


evalInContext :: String -> V8Context -> IO V8Value
evalInContext str ctx = newCString str >>= (`c_eval_in_context` ctx)


foreign import ccall safe "cbits/haskell-v8.h new_context" 
  c_new_context :: IO V8Context


foreign import ccall safe "cbits/haskell-v8.h eval_in_context" 
  c_eval_in_context :: CString -> V8Context -> IO V8Value
