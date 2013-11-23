{-# LANGUAGE ForeignFunctionInterface #-}

module V8
    ( 
        newIsolate
    ,   newContext
    ,   evalInContext
    )
  where


import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String


type V8Isolate = ()
type V8Context = ()
type V8Value = ()

data Isolate = Isolate !(ForeignPtr V8Isolate)
data Context = Context !(ForeignPtr V8Context)
data Value = Value !(ForeignPtr V8Value)


newIsolate :: IO Isolate
newIsolate = do
        isoPtr <- c_new_isolate
        fIsoPtr <- newForeignPtr c_free_isolate isoPtr
        return $ Isolate fIsoPtr


newContext :: Isolate -> IO Context
newContext (Isolate fIsoPtr) = withForeignPtr fIsoPtr newCtx
    where
        newCtx isoPtr = do
            ctxPtr <- c_new_context isoPtr
            fCtxPtr <- newForeignPtr c_free_context ctxPtr
            return $ Context fCtxPtr


evalInContext :: String -> Context -> IO Value
evalInContext str (Context fCtxPtr) = withForeignPtr fCtxPtr eval
    where
        eval ctxPtr = do
            cStr <- newCString str
            valPtr <- c_eval_in_context cStr ctxPtr
            fValPtr <- newForeignPtr c_free_value valPtr
            return $ Value fValPtr


foreign import ccall "cbits/haskell-v8.h &free_isolate"
    c_free_isolate :: FunPtr (Ptr V8Isolate -> IO ())


foreign import ccall "cbits/haskell-v8.h &free_context"
    c_free_context :: FunPtr (Ptr V8Context -> IO ())


foreign import ccall "cbits/haskell-v8.h &free_value"
    c_free_value :: FunPtr (Ptr V8Value -> IO ())


foreign import ccall safe "cbits/haskell-v8.h new_isolate" 
    c_new_isolate :: IO (Ptr V8Isolate)


foreign import ccall safe "cbits/haskell-v8.h new_context" 
    c_new_context :: Ptr V8Isolate -> IO (Ptr V8Context)


foreign import ccall safe "cbits/haskell-v8.h eval_in_context" 
  c_eval_in_context :: CString -> Ptr V8Context -> IO (Ptr V8Value)
