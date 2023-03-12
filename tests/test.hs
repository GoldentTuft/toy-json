{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.Foldable (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ToyJson as TJ
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ boolTests,
      numberTests,
      nullTests,
      stringTests,
      arrayTests,
      objectTests,
      parseTests
    ]

getVal :: IResult i r -> Maybe r
getVal res =
  case res of
    Done _ r ->
      Just r
    _ ->
      Nothing

data Pat a = Pat T.Text (Maybe a)

testCaseHelp :: (Eq a, Show a) => Parser a -> Pat a -> TestTree
testCaseHelp parser (Pat input expect) =
  testCase (T.unpack input) $ do
    let res = parse parser input `feed` ""
    getVal res @?= expect

boolTests :: TestTree
boolTests = do
  let pats =
        [ Pat "true" (Just $ TJ.TjBool True),
          Pat "false" (Just $ TJ.TjBool False),
          Pat "teru" Nothing,
          Pat "True" Nothing
        ]
  testGroup "Bool" $ testCaseHelp TJ.tjBool <$> pats

numberTests :: TestTree
numberTests = do
  let pats =
        [ Pat "5" (Just $ TJ.TjNumber 5),
          Pat "1.1" (Just $ TJ.TjNumber 1.1),
          Pat "-1.09" (Just $ TJ.TjNumber (-1.09))
        ]
  testGroup "Number" $ testCaseHelp TJ.tjNumber <$> pats

nullTests :: TestTree
nullTests = do
  let pats =
        [ Pat "null" (Just TJ.TjNull),
          Pat "Null" Nothing
        ]
  testGroup "Null" $ testCaseHelp TJ.tjNull <$> pats

stringTests :: TestTree
stringTests = do
  let pats =
        [ Pat "\"a\\\"b\"" (Just $ TJ.TjString "a\"b"),
          Pat "\" \\\n;\"" (Just $ TJ.TjString " \n;")
        ]
  testGroup "String" $ testCaseHelp TJ.tjString <$> pats

arrayTests :: TestTree
arrayTests = do
  let pats =
        [ Pat "[5, true,null ]" (Just $ TJ.TjArray [TJ.TjNumber 5.0, TJ.TjBool True, TJ.TjNull]),
          Pat "[\"abc\", -3.0, 0]" (Just $ TJ.TjArray [TJ.TjString "abc", TJ.TjNumber (-3.0), TJ.TjNumber 0.0]),
          Pat
            "[\n\
            \\t10.0,\n\
            \\t11\n\
            \]"
            (Just $ TJ.TjArray [TJ.TjNumber 10.0, TJ.TjNumber 11.0])
        ]
  testGroup "Array" $ testCaseHelp TJ.tjArray <$> pats

objectTests :: TestTree
objectTests = do
  let pats =
        [ Pat "{\"KEY\": 5}" (Just $ TJ.TjObject [("KEY", TJ.TjNumber 5.0)]),
          Pat "{\"KEY1\": 5, \"KEY2\": \"10\"}" (Just $ TJ.TjObject [("KEY1", TJ.TjNumber 5.0), ("KEY2", TJ.TjString "10")]),
          Pat
            "{\n\
            \\"寿司\": true,\n\
            \\"KEY2\": [10, 20]\n\
            \}"
            (Just $ TJ.TjObject [("寿司", TJ.TjBool True), ("KEY2", TJ.TjArray [TJ.TjNumber 10.0, TJ.TjNumber 20.0])]),
          Pat
            "{ \"123\": 456\n\
            \, \"KEY\": {\"KEY_NEST\": \"VAL_NEST\"}\n\
            \}"
            (Just $ TJ.TjObject [("123", TJ.TjNumber 456.0), ("KEY", TJ.TjObject [("KEY_NEST", TJ.TjString "VAL_NEST")])])
        ]
  testGroup "Object" $ testCaseHelp TJ.tjObject <$> pats

parseTests :: TestTree
parseTests =
  testGroup
    "parse"
    [testParse]

testParse :: TestTree
testParse = testCase "" $ do
  sample1 <- T.readFile ("." </> "tests" </> "sample4.json")
  val <- case TJ.fromText sample1 of
    Done _ r ->
      return r
    _ ->
      fail ""
  case val of
    TJ.TjArray (_ : TJ.TjObject fields : _) -> do
      let field = find (\fd -> "languages" == fst fd) fields
      case field of
        Just (_, TJ.TjArray langs) ->
          case langs of
            [_, TJ.TjString lang] ->
              assertEqual "lang" "Japanese" lang
            _ ->
              assertFailure "100"
        _ ->
          assertFailure "200"
    _ ->
      assertFailure "300"
