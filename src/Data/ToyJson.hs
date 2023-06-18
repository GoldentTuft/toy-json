{-# LANGUAGE OverloadedStrings #-}

module Data.ToyJson where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text hiding (take)
import Data.Functor (($>))
import qualified Data.Text as T

data TjValue
  = TjNull
  | TjObject [(TjKey, TjValue)]
  | TjArray [TjValue]
  | TjString T.Text
  | TjNumber Double
  | TjBool Bool
  deriving (Eq, Show)

type TjKey = T.Text

fromText :: T.Text -> Result TjValue
fromText text = parse tjValue text `feed` ""

tjValue :: Parser TjValue
tjValue =
  tjNull
    <|> tjObject
    <|> tjArray
    <|> tjString
    <|> tjNumber
    <|> tjBool

tjNull :: Parser TjValue
tjNull =
  TjNull <$ string "null"

tjObject :: Parser TjValue
tjObject =
  TjObject
    <$> choice
      [ (char '{' *> skipSpace *> char '}') $> [],
        char '{' *> fields [] <* char '}'
      ]
  where
    fields acc = do
      skipSpace
      keyVal <- tjString
      case keyVal of
        TjString key -> do
          skipSpace
          _ <- char ':'
          skipSpace
          val <- tjValue
          skipSpace
          c <- peekChar'
          case c of
            '}' -> return (acc ++ [(key, val)])
            _ -> anyChar >> fields (acc ++ [(key, val)])
        _ -> fail "undefined error at field key"

tjArray :: Parser TjValue
tjArray =
  TjArray
    <$> choice
      [ (char '[' *> skipSpace *> char ']') $> [],
        char '[' *> vals [] <* char ']'
      ]
  where
    vals acc = do
      skipSpace
      val <- tjValue
      skipSpace
      c <- peekChar'
      case c of
        ',' -> anyChar >> vals (acc ++ [val])
        ']' -> return (acc ++ [val])
        _ -> fail "expect , or ]"

tjString :: Parser TjValue
tjString =
  TjString . T.pack <$> (char '"' *> many' (escape <|> notChar '"') <* char '"')
  where
    escape = char '\\' *> fmap conv anyChar
    conv 'n' = '\n'
    conv 'r' = '\r'
    conv 't' = '\t'
    conv c = c

tjNumber :: Parser TjValue
tjNumber =
  TjNumber <$> signed double

tjBool :: Parser TjValue
tjBool =
  TjBool <$> bool_
  where
    bool_ =
      (string "true" >> return True)
        <|> (string "false" >> return False)
