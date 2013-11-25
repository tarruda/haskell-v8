{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module V8
    ( 
        newIsolate
    ,   newContext
    ,   evalInContext
    )
  where


import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

#include <haskell-v8-common.h>

newtype V8Isolate = V8Isolate (ForeignPtr ())
newtype V8Context = V8Context (ForeignPtr ())

newtype V8Undefined = V8Undefined (ForeignPtr ())
newtype V8Null = V8Null (ForeignPtr ())
newtype V8True = V8True (ForeignPtr ())
newtype V8False = V8False (ForeignPtr ())
newtype V8Number = V8Number (ForeignPtr ())
newtype V8String = V8String (ForeignPtr ())
newtype V8Array = V8Array (ForeignPtr ())
newtype V8Object = V8Object (ForeignPtr ())
newtype V8RegExp = V8RegExp (ForeignPtr ())
newtype V8Date = V8Date (ForeignPtr ())
newtype V8Unknown = V8Unknown (ForeignPtr ())


data JSValue =
        JSUndefined V8Undefined
    |   JSNull V8Null
    |   JSTrue V8True
    |   JSFalse V8False
    |   JSNumber V8Number
    |   JSString V8String
    |   JSArray V8Array
    |   JSObject V8Object
    |   JSRegExp V8RegExp
    |   JSDate V8Date
    |   JSUnknown V8Unknown


instance Show JSValue where
    show = unsafePerformIO . v8ToString


newIsolate :: IO V8Isolate
newIsolate = do
    isoPtr <- c_new_isolate
    fIsoPtr <- newForeignPtr c_free_isolate isoPtr
    return $ V8Isolate fIsoPtr


newContext :: V8Isolate -> IO V8Context
newContext (V8Isolate fIsoPtr) = withForeignPtr fIsoPtr newCtx
    where
        newCtx isoPtr = do
            ctxPtr <- c_new_context isoPtr
            fCtxPtr <- newForeignPtr c_free_context ctxPtr
            return $ V8Context fCtxPtr


evalInContext :: String -> V8Context -> IO JSValue
evalInContext str (V8Context fCtxPtr) = withForeignPtr fCtxPtr eval
    where
        eval ctxPtr = do
            cStr <- newCString str
            typeValPtr <- c_eval_in_context cStr ctxPtr
            (TypeValue t valPtr) <- peek typeValPtr
            fValPtr <- newForeignPtr c_free_value valPtr
            return $ case t of
                #{const V8TYPE_UNDEFINED} -> JSUndefined (V8Undefined fValPtr)
                #{const V8TYPE_NULL}      -> JSNull (V8Null fValPtr)
                #{const V8TYPE_TRUE}      -> JSTrue (V8True fValPtr)
                #{const V8TYPE_FALSE}     -> JSFalse (V8False fValPtr)
                #{const V8TYPE_NUMBER}    -> JSNumber (V8Number fValPtr)
                #{const V8TYPE_STRING}    -> JSString (V8String fValPtr)
                #{const V8TYPE_ARRAY}     -> JSArray (V8Array fValPtr)
                #{const V8TYPE_OBJECT}    -> JSObject (V8Object fValPtr)
                #{const V8TYPE_REGEXP}    -> JSRegExp (V8RegExp fValPtr)
                #{const V8TYPE_DATE}      -> JSDate (V8Date fValPtr)
                _                         -> JSUnknown (V8Unknown fValPtr)


v8ToString :: JSValue -> IO String
v8ToString = toStr
    where
        toStr (JSUndefined (V8Undefined fValPtr)) = marshalPtr fValPtr
        toStr (JSNull (V8Null fValPtr)) = marshalPtr fValPtr
        toStr (JSTrue (V8True fValPtr)) = marshalPtr fValPtr
        toStr (JSFalse (V8False fValPtr)) = marshalPtr fValPtr
        toStr (JSNumber (V8Number fValPtr)) = marshalPtr fValPtr
        toStr (JSString (V8String fValPtr)) = marshalPtr fValPtr
        toStr (JSArray (V8Array fValPtr)) = marshalPtr fValPtr
        toStr (JSObject (V8Object fValPtr)) = marshalPtr fValPtr
        toStr (JSRegExp (V8RegExp fValPtr)) = marshalPtr fValPtr
        toStr (JSDate (V8Date fValPtr)) = marshalPtr fValPtr
        toStr _ = return "<Uknown V8 type>"
        marshalPtr fValPtr = withForeignPtr fValPtr withPtr
        withPtr valPtr = do
            cstr <- c_v8_to_string valPtr
            rv <- peekCString cstr
            free cstr
            return rv




foreign import ccall "cbits/haskell-v8.h &free_isolate"
    c_free_isolate :: FunPtr (Ptr () -> IO ())


foreign import ccall "cbits/haskell-v8.h &free_context"
    c_free_context :: FunPtr (Ptr () -> IO ())


foreign import ccall "cbits/haskell-v8.h &free_value"
    c_free_value :: FunPtr (Ptr () -> IO ())


foreign import ccall safe "cbits/haskell-v8.h new_isolate" 
    c_new_isolate :: IO (Ptr ())


foreign import ccall safe "cbits/haskell-v8.h new_context" 
    c_new_context :: Ptr () -> IO (Ptr ())


foreign import ccall safe "cbits/haskell-v8.h eval_in_context" 
  c_eval_in_context :: CString -> Ptr () -> IO (Ptr TypeValue)


foreign import ccall safe "cbits/haskell-v8.h v8_to_string" 
  c_v8_to_string :: Ptr () -> IO CString



data TypeValue = TypeValue CInt (Ptr ())

instance Storable TypeValue where
    sizeOf _ = (#size type_value)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        type' <- (#peek type_value, type) ptr
        value' <- (#peek type_value, value) ptr
        return $ TypeValue type' value'
    poke ptr (TypeValue type' value') = do
        (#poke type_value, type) ptr type'
        (#poke type_value, value) ptr value'
