import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import SparkFFI

main :: IO ()
main = do
  putStrLn "Testing canonicalize (no manual init)..."
  result <- sparkCanonicalize (T.pack "https://example.com/test")
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right url -> putStrLn $ "Success: " ++ T.unpack url
  
  putStrLn "Testing short code..."
  let secret = BS.pack "abcdef1234567890abcdef1234567890"
  code <- sparkMakeShortCode secret (T.pack "https://example.com/test")
  putStrLn $ "Short code: " ++ T.unpack code
  
  putStrLn "Done"
