-- File auto generated by purescript-bridge! --
module Wallet.Emulator.Wallet where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(Proxy))

newtype Wallet = Wallet
  { prettyWalletName :: Maybe String
  , getWalletId :: String
  }

derive instance Eq Wallet

instance Show Wallet where
  show a = genericShow a

instance EncodeJson Wallet where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { prettyWalletName: (E.maybe E.value) :: _ (Maybe String)
        , getWalletId: E.value :: _ String
        }
    )

instance DecodeJson Wallet where
  decodeJson = defer \_ -> D.decode $
    ( Wallet <$> D.record "Wallet"
        { prettyWalletName: (D.maybe D.value) :: _ (Maybe String)
        , getWalletId: D.value :: _ String
        }
    )

derive instance Generic Wallet _

derive instance Newtype Wallet _

--------------------------------------------------------------------------------

_Wallet
  :: Iso' Wallet
       { prettyWalletName :: Maybe String, getWalletId :: String }
_Wallet = _Newtype
