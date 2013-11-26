{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module V8
    ( 
        newIsolate
    ,   newContext
    ,   evalInContext
    ,   toString
    )
  where


import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

#include <haskell-v8-common.h>

newtype V8Isolate = V8Isolate (ForeignPtr ()) deriving (Show)
newtype V8Context = V8Context (ForeignPtr ()) deriving (Show)
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
-- newtype V8Exception = V8Exception (ForeignPtr ())


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
    -- |   JSException V8Exception
    |   JSUnknown V8Unknown


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


evalInContext :: String -> V8Context -> IO (Either JSValue JSValue)
evalInContext str (V8Context fCtxPtr) = withForeignPtr fCtxPtr eval
    where
        eval ctxPtr = do
            cStr <- newCString str
            typeValPtr <- c_eval_in_context cStr ctxPtr
            (V8Result typ isException valPtr) <- peek typeValPtr
            fValPtr <- newForeignPtr c_free_value valPtr
            return $ case typ of
                #{const V8TYPE_UNDEFINED} -> if isException == 1
                    then Left (JSUndefined (V8Undefined fValPtr))
                    else Right (JSUndefined (V8Undefined fValPtr))
                #{const V8TYPE_NULL}      -> if isException == 1
                    then Left (JSNull (V8Null fValPtr))
                    else Right (JSNull (V8Null fValPtr))
                #{const V8TYPE_TRUE}      -> if isException == 1
                    then Left (JSTrue (V8True fValPtr))
                    else Right (JSTrue (V8True fValPtr))
                #{const V8TYPE_FALSE}     -> if isException == 1
                    then Left (JSFalse (V8False fValPtr))
                    else Right (JSFalse (V8False fValPtr))
                #{const V8TYPE_NUMBER}    -> if isException == 1
                    then Left (JSNumber (V8Number fValPtr))
                    else Right (JSNumber (V8Number fValPtr))
                #{const V8TYPE_STRING}    -> if isException == 1
                    then Left (JSString (V8String fValPtr))
                    else Right (JSString (V8String fValPtr))
                #{const V8TYPE_ARRAY}     -> if isException == 1
                    then Left (JSArray (V8Array fValPtr))
                    else Right (JSArray (V8Array fValPtr))
                #{const V8TYPE_OBJECT}    -> if isException == 1
                    then Left (JSObject (V8Object fValPtr))
                    else Right (JSObject (V8Object fValPtr))
                #{const V8TYPE_REGEXP}    -> if isException == 1
                    then Left (JSRegExp (V8RegExp fValPtr))
                    else Right (JSRegExp (V8RegExp fValPtr))
                #{const V8TYPE_DATE}      -> if isException == 1
                    then Left (JSDate (V8Date fValPtr))
                    else Right (JSDate (V8Date fValPtr))
                _                         -> if isException == 1
                    then Left (JSUnknown (V8Unknown fValPtr))
                    else Right (JSUnknown (V8Unknown fValPtr))


toString :: JSValue -> IO String
toString = toStr
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
        toStr _ = return "<Unknown V8 type>"
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
  c_eval_in_context :: CString -> Ptr () -> IO (Ptr V8Result)


foreign import ccall safe "cbits/haskell-v8.h v8_to_string" 
  c_v8_to_string :: Ptr () -> IO CString



data V8Result = V8Result CInt CInt (Ptr ())

instance Storable V8Result where
    sizeOf _ = (#size v8_result)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        type' <- (#peek v8_result, type) ptr
        thrown' <- (#peek v8_result, thrown) ptr
        value' <- (#peek v8_result, value) ptr
        return $ V8Result type' thrown' value'
    poke ptr (V8Result type' thrown' value') = do
        (#poke v8_result, type) ptr type'
        (#poke v8_result, thrown) ptr thrown'
        (#poke v8_result, value) ptr value'
