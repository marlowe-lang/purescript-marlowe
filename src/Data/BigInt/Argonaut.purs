module Data.BigInt.Argonaut
  ( module Data.BigInt
  , BigInt(..)
  , BigIntJson
  , Reviver
  , abs
  , and
  , digitsInBase
  , even
  , fromBase
  , fromInt
  , fromNumber
  , fromString
  , JsonString(..)
  , not
  , odd
  , or
  , patchers
  , pow
  , prime
  , quot
  , rem
  , replacer
  , shl
  , shr
  , toBase'
  , toBase
  , toNonEmptyString
  , toNumber
  , toString
  , xor
  ) where

import Prelude

import Data.Argonaut (fromString) as Argonaut
import Data.Argonaut.Aeson (maybeToEither)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.BigInt (BaseDigits)
import Data.BigInt as BI
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

newtype BigInt = BigInt BI.BigInt

derive instance genericBigInt :: Generic BigInt _

derive instance newtypeBigInt :: Newtype BigInt _

derive instance eqBigInt :: Eq BigInt

derive instance ordBigInt :: Ord BigInt

derive newtype instance showBigInt :: Show BigInt

derive newtype instance semiringBigInt :: Semiring BigInt

derive newtype instance ringBigInt :: Ring BigInt

derive newtype instance commutativeRingBigInt :: CommutativeRing BigInt

derive newtype instance euclideanRingBigInt :: EuclideanRing BigInt

foreign import decodeBigInt :: forall a. Fn3 a (BI.BigInt -> a) Json a

foreign import encodeBigInt :: BI.BigInt -> Json

instance decodeJsonBigInt :: DecodeJson BigInt where
  decodeJson =
    maybeToEither (TypeMismatch "BigInt or Number")
      <<< runFn3 decodeBigInt Nothing (Just <<< BigInt)

instance encodeJsonBitInt :: EncodeJson BigInt where
  encodeJson = encodeBigInt <<< unwrap

fromString :: String -> Maybe BigInt
fromString = map BigInt <<< BI.fromString

fromBase :: Int -> String -> Maybe BigInt
fromBase base = map BigInt <<< BI.fromBase base

fromInt :: Int -> BigInt
fromInt = BigInt <<< BI.fromInt

fromNumber :: Number -> Maybe BigInt
fromNumber = map BigInt <<< BI.fromNumber

toString :: BigInt -> String
toString = BI.toString <<< unwrap

toNonEmptyString :: BigInt -> NonEmptyString
toNonEmptyString = BI.toNonEmptyString <<< unwrap

toBase :: Int -> BigInt -> String
toBase base = BI.toBase base <<< unwrap

toBase' :: Int -> BigInt -> NonEmptyString
toBase' base = BI.toBase' base <<< unwrap

digitsInBase :: Int -> BigInt -> BaseDigits
digitsInBase base = BI.digitsInBase base <<< unwrap

toNumber :: BigInt -> Number
toNumber = BI.toNumber <<< unwrap

abs :: BigInt -> BigInt
abs = over BigInt BI.abs

even :: BigInt -> Boolean
even = BI.even <<< unwrap

odd :: BigInt -> Boolean
odd = BI.odd <<< unwrap

prime :: BigInt -> Boolean
prime = BI.prime <<< unwrap

pow :: BigInt -> BigInt -> BigInt
pow a n = BigInt $ BI.pow (unwrap a) (unwrap n)

not :: BigInt -> BigInt
not = over BigInt BI.not

or :: BigInt -> BigInt -> BigInt
or a b = BigInt $ BI.or (unwrap a) (unwrap b)

xor :: BigInt -> BigInt -> BigInt
xor a b = BigInt $ BI.xor (unwrap a) (unwrap b)

and :: BigInt -> BigInt -> BigInt
and a b = BigInt $ BI.and (unwrap a) (unwrap b)

shl :: BigInt -> Number -> BigInt
shl a = BigInt <<< BI.shl (unwrap a)

shr :: BigInt -> Number -> BigInt
shr a = BigInt <<< BI.shr (unwrap a)

quot :: BigInt -> BigInt -> BigInt
quot a b = BigInt $ BI.quot (unwrap a) (unwrap b)

rem :: BigInt -> BigInt -> BigInt
rem a b = BigInt $ BI.rem (unwrap a) (unwrap b)

-- A Json which contains possibly `BigInt` values.
-- This opaque type can be used to mark the values which contain `BigInt`.
-- Not sure if we need it other than to make type signatures more clear.
foreign import data BigIntJson :: Type

fromJson :: Json -> BigIntJson
fromJson = unsafeCoerce

toBigInt :: BigIntJson -> Maybe BI.BigInt
toBigInt json =
  if isBigInt json then Just <<< unsafeCoerce $ json
  else Nothing

-- | A reviver function which can be passed to the JSON parser.
-- | Please check the official JS JSON.parse documentation for more details.
type Reviver = String -> Json -> Json
-- | String which contains serialized JSON.
newtype JsonString = JsonString String
type ParseResultHandlers a =
  { failure :: String -> a, success :: BigIntJson -> a }

foreign import patchersImpl
  :: { patchStringify :: Effect Unit
     , patchParse :: Effect Unit
     , parseImpl :: forall a. Fn3 (ParseResultHandlers a) Reviver JsonString a
     }

-- The actual patchers which you **should use in your code** in order to
-- properly stringify/parse `BigInt` values in JSON.
patchers
  :: { patchStringify :: Effect Unit
     , patchParse :: Effect Unit
     , parse :: Reviver -> JsonString -> Either String BigIntJson
     }
patchers = do
  let
    eitherHandlers = { failure: Left, success: Right }
    parse = runFn3 patchersImpl.parseImpl eitherHandlers
  { parse
  , patchStringify: patchersImpl.patchStringify
  , patchParse: patchersImpl.patchParse
  }

foreign import isBigInt :: forall a. a -> Boolean

replacer :: String -> BigIntJson -> BigIntJson
replacer _ value = case toBigInt value of
  Just bigInt -> fromJson <<< Argonaut.fromString <<< show $ bigInt
  Nothing -> value

