{-# LANGUAGE TemplateHaskell #-}
module Compact where

import Language.Haskell.TH hiding (Lift(..))
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Data.Compact
import Data.Compact.Serialize
import Data.ByteString.Handle
import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (bsToExp)

liftBS :: B.ByteString -> TExpQ B.ByteString
liftBS = unsafeTExpCoerce . bsToExp

{-# NOINLINE persist #-}
persist :: Typeable a => B.ByteString -> a
persist bs = unsafePerformIO $ do
--      h <- readHandle True (BL.fromStrict bs)
--      res <- hUnsafeGetCompact h
        B.writeFile "/tmp/compact-read" bs
        res <- unsafeReadCompact "/tmp/compact-read"
        return $ case res of
          Left err -> error err
          Right c -> getCompact c


lift :: Typeable a => a -> TExpQ a
lift x = do
  bs <- runIO $ do
    -- Put the value into a compact region
    c <- compact x
    -- Write the compact region to a file
    writeCompact "/tmp/compact-test" c
    B.readFile "/tmp/compact-test"
    {-
    (bs, ()) <- writeHandle True (\h -> hPutCompact h c)
    return bs
    -}
  [|| persist $$(liftBS bs) ||]




