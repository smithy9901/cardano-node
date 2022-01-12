module Node where

import           Data.Aeson (eitherDecodeStrict)
import           Cardano.Node.Run (runNode)

import           Foreign.C (CString, peekCString)
import           Data.ByteString.Char8 (pack)

foreign export ccall "runNode" crunNode :: CString -> IO ()

crunNode :: CString -> IO ()
crunNode config = pack <$> peekCString config >>= \config' ->
    case (eitherDecodeStrict config') of
        Left err -> putStrLn err
        Right pnc -> runNode pnc
