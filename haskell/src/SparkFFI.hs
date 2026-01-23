{-# LANGUAGE ForeignFunctionInterface #-}

module SparkFFI
  ( initSpark
  , finalizeSpark
  , sparkCanonicalize
  , sparkMakeShortCode
  , FFIResultCode(..)
  ) where

import Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

-- | FFI result codes matching Core_FFI.FFI_Result_Code
data FFIResultCode
  = FFISuccess
  | FFIInvalidLength
  | FFIInvalidScheme
  | FFIInvalidHost
  | FFIPrivateAddress
  | FFICredentialsPresent
  | FFIInvalidCharacters
  deriving (Show, Eq)

fromCInt :: CInt -> FFIResultCode
fromCInt 0 = FFISuccess
fromCInt 1 = FFIInvalidLength
fromCInt 2 = FFIInvalidScheme
fromCInt 3 = FFIInvalidHost
fromCInt 4 = FFIPrivateAddress
fromCInt 5 = FFICredentialsPresent
fromCInt 6 = FFIInvalidCharacters
fromCInt _ = FFIInvalidCharacters

toValidationError :: FFIResultCode -> ValidationError
toValidationError FFISuccess            = error "FFISuccess should not be converted to error"
toValidationError FFIInvalidLength      = InvalidLength
toValidationError FFIInvalidScheme      = InvalidScheme
toValidationError FFIInvalidHost        = InvalidHost
toValidationError FFIPrivateAddress     = PrivateAddress
toValidationError FFICredentialsPresent = CredentialsPresent
toValidationError FFIInvalidCharacters  = InvalidCharacters

-- | Foreign import for Ada runtime initialization
foreign import ccall unsafe "hadlink_init"
  c_hadlink_init :: IO ()

-- | Foreign import for Ada runtime finalization
foreign import ccall unsafe "hadlink_final"
  c_hadlink_final :: IO ()

-- | Foreign import for hadlink_canonicalize
-- int hadlink_canonicalize(const char* input, char* output, size_t* output_len)
foreign import ccall unsafe "hadlink_canonicalize"
  c_hadlink_canonicalize :: CString -> CString -> Ptr CSize -> IO CInt

-- | Foreign import for hadlink_make_short_code
-- int hadlink_make_short_code(const char* url, const char* secret, char* output)
foreign import ccall unsafe "hadlink_make_short_code"
  c_hadlink_make_short_code :: CString -> CString -> CString -> IO CInt

-- | Initialize Ada runtime (call once at program startup)
initSpark :: IO ()
initSpark = c_hadlink_init

-- | Finalize Ada runtime (call once at program shutdown)
finalizeSpark :: IO ()
finalizeSpark = c_hadlink_final

-- | Canonicalize a URL using SPARK core
-- Calls hadlink_canonicalize and returns either an error or the canonical URL
sparkCanonicalize :: T.Text -> IO (Either ValidationError T.Text)
sparkCanonicalize rawUrl = do
  let inputBS = TE.encodeUtf8 rawUrl
  BS.useAsCString inputBS $ \inputPtr -> do
    -- Allocate output buffer (max 2048 bytes + null terminator)
    allocaBytes 2049 $ \outputPtr -> do
      alloca $ \outputLenPtr -> do
        resultCode <- c_hadlink_canonicalize inputPtr outputPtr outputLenPtr
        outputLen <- peek outputLenPtr
        
        let ffiResult = fromCInt resultCode
        
        if ffiResult == FFISuccess
          then do
            -- Read the output string
            outputStr <- peekCStringLen (outputPtr, fromIntegral outputLen)
            return $ Right (T.pack outputStr)
          else
            return $ Left (toValidationError ffiResult)

-- | Generate a short code using SPARK core
-- Calls hadlink_make_short_code and returns the short code
-- Throws error if the operation fails
sparkMakeShortCode :: BS.ByteString -> T.Text -> IO T.Text
sparkMakeShortCode secret canonicalUrl = do
  let urlBS = TE.encodeUtf8 canonicalUrl
  
  -- Ensure secret is exactly 32 bytes
  when (BS.length secret /= 32) $
    error "Secret must be exactly 32 bytes"
  
  BS.useAsCString urlBS $ \urlPtr -> do
    BS.useAsCString secret $ \secretPtr -> do
      -- Allocate output buffer (8 bytes + null terminator)
      allocaBytes 9 $ \outputPtr -> do
        resultCode <- c_hadlink_make_short_code urlPtr secretPtr outputPtr
        
        when (resultCode /= 0) $
          error $ "hadlink_make_short_code failed with code: " ++ show resultCode
        
        -- Read the output string (should be exactly 8 characters)
        outputStr <- peekCString outputPtr
        return $ T.pack outputStr
  where
    when True action = action
    when False _ = return ()
