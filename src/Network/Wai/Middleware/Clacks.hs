{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Clacks
    ( clacks
    , Clacks(..)
    , gnuTerryPratchett
    , clacksHeaderName
    )
where

import           Prelude.Compat

import           Data.List.NonEmpty.Compat      ( NonEmpty(..)
                                                , toList
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Network.HTTP.Types.Header      ( ResponseHeaders
                                                , HeaderName
                                                )
import           Network.Wai                    ( Middleware
                                                , modifyResponse
                                                , mapResponseHeaders
                                                )

import qualified Data.ByteString               as BS
import qualified Data.CaseInsensitive          as CI
import qualified Data.List                     as L
import qualified Data.Text                     as T


-- | A "Network.Wai" 'Middleware' that adds a @X-Clacks-Overhead@ header
-- containing the messages in the 'Clacks' configuration.
clacks :: Clacks -> Middleware
clacks settings = modifyResponse $ mapResponseHeaders addHeader
  where
    headerContents :: BS.ByteString
    headerContents =
        encodeUtf8 . T.intercalate "," . toList $ clacksMessages settings
    addHeader :: ResponseHeaders -> ResponseHeaders
    addHeader hs = case L.find ((== clacksHeaderName) . fst) hs of
        Nothing      -> (clacksHeaderName, headerContents) : hs
        Just (_, "") -> (clacksHeaderName, headerContents) : hs
        Just (name, contents) ->
            (name, contents <> "," <> headerContents)
                : filter ((/= clacksHeaderName) . fst) hs

-- | Configuration for the Clacks WAI Middleware.
newtype Clacks =
    Clacks
        { clacksMessages :: NonEmpty T.Text
        -- ^ The Clacks Messages to Include in the Header.
        } deriving (Show, Read, Eq)


-- | Sends a Clacks message of @GNU Terry Pratchett@.
gnuTerryPratchett :: Clacks
gnuTerryPratchett = Clacks $ "GNU Terry Pratchett" :| []


-- | The name of the Clacks header.
clacksHeaderName :: HeaderName
clacksHeaderName = CI.mk "X-Clacks-Overhead"
