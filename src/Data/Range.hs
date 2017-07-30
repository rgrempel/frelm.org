{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Range where

import ClassyPrelude hiding ((<|>))
import Database.Esqueleto (Value)
import Database.Esqueleto.Internal.Sql
import Database.Persist (PersistValue(..))
import Text.Parsec

valueInRange ::
       SqlExpr (Value a) -> SqlExpr (Value (Range a)) -> SqlExpr (Value Bool)
valueInRange = unsafeSqlBinOp " <@ "

data Bound a
    = Inclusive a
    | Exclusive a
    | Unspecified
    deriving (Eq, Show)

data Range a =
    Range (Bound a)
          (Bound a)
    deriving (Eq, Show)

contains :: Ord a => Range a -> a -> Bool
contains (Range lowerBound upperBound) v = initial && final
  where
    initial =
        case lowerBound of
            Inclusive a -> v >= a
            Exclusive a -> v > a
            Unspecified -> True
    final =
        case upperBound of
            Inclusive a -> v <= a
            Exclusive a -> v < a
            Unspecified -> True

-- | Parses the kind of version string you see in an elm-package.json
-- |
-- | So, something like `5.0.0 <= v < 6.0.1`
elmPackageRange :: Parsec Text () a -> Parsec Text () (Range a)
elmPackageRange subparser = do
    spaces
    lowerBound <- subparser
    spaces
    lowerConstructor <- parseElmComparator
    spaces >> char 'v' >> spaces
    upperConstructor <- parseElmComparator
    spaces
    upperBound <- subparser
    spaces >> eof
    pure $ Range (lowerConstructor lowerBound) (upperConstructor upperBound)

parseElmComparator :: Parsec Text () (a -> Bound a)
parseElmComparator = char '<' >> option Exclusive (const Inclusive <$> char '=')

showElmPackageRange :: (a -> Text) -> Range a -> Text
showElmPackageRange func (Range lowerBound upperBound) = initial <> "v" <> final
  where
    initial =
        case lowerBound of
            Inclusive a -> func a <> " <= "
            Exclusive a -> func a <> " < "
            Unspecified -> ""
    final =
        case upperBound of
            Inclusive a -> " <= " <> func a
            Exclusive a -> " < " <> func a
            Unspecified -> ""

-- | Parses the Postgres way of representing a range
postgresRange :: Parsec Text () a -> Parsec Text () (Range a)
postgresRange subparser = parseEmpty <|> parseRange
  where
    parseEmpty = const (Range Unspecified Unspecified) <$> string "empty"
    parseRange = do
        lowerConstructor <-
            (const Inclusive <$> char '[') <|> (const Exclusive <$> char '(')
        lowerBound <- subparser
        void $ char ','
        upperBound <- subparser
        upperConstructor <-
            (const Inclusive <$> char ']') <|> (const Exclusive <$> char ')')
        pure $ Range (lowerConstructor lowerBound) (upperConstructor upperBound)

makePersistValue :: (a -> ByteString) -> Range a -> PersistValue
makePersistValue toByteString (Range lowerBound upperBound) =
    PersistDbSpecific $ initial <> "," <> final
  where
    initial =
        case lowerBound of
            Inclusive a -> "[" <> toByteString a
            Exclusive a -> "(" <> toByteString a
            Unspecified -> "["
    final =
        case upperBound of
            Inclusive a -> toByteString a <> "]"
            Exclusive a -> toByteString a <> ")"
            Unspecified -> "]"

readPersistValue :: Parsec Text () a -> PersistValue -> Either Text (Range a)
readPersistValue subparser value =
    case value of
        PersistDbSpecific str ->
            first tshow $ parse (postgresRange subparser) "" (decodeUtf8 str)
        _ -> Left $ "Got unexpected persist value: " ++ tshow value
