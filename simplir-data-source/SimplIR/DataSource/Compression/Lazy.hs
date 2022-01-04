module SimplIR.DataSource.Compression.Lazy
    ( -- * Compression schemes
      Compression(..)
    , decompress
    , decompressWith
      -- * Utilities
    , guessCompressionFromFileName
    , detectCompression
    ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Compression.Lzma as Lzma
import SimplIR.DataSource.Compression

decompress :: BSL.ByteString -> BSL.ByteString
decompress bs = 
    let header = BSL.toStrict $ BSL.take 100 bs
        comp = detectCompression header
     in decompressWith comp bs

decompressWith :: Maybe Compression -> BSL.ByteString -> BSL.ByteString
decompressWith Nothing      = id
decompressWith (Just GZip)  = GZip.decompress
decompressWith (Just BZip2) = BZip.decompress
decompressWith (Just Lzma)  = Lzma.decompress

