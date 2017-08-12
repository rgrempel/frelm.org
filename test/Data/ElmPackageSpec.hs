{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.ElmPackageSpec (spec) where


import Data.Aeson
import Data.ElmPackage
import qualified Data.Map
import Data.Range
import Data.SemVer
import NeatInterpolation (text)
import TestImport


spec :: Spec
spec = withApp $
    describe "FromJSON" $
        it "parses a typical example" $
            assertEq "gets expected result" (Right typicalExampleParsed) $
                eitherDecodeStrict typicalExampleJSON


typicalExampleParsed :: ElmPackage
typicalExampleParsed =
    ElmPackage
        { elmPackageVersion = version 1 0 0 [] []
        , elmPackageSummary = "Dict with arbitrary ordering (like List)"
        , elmPackageRepository = "https://github.com/Gizra/elm-dictlist.git"
        , elmPackageLicense = "MIT"
        , elmPackageModules = ["DictList"]
        , elmPackageSourceDirectories = ["src"]
        , elmPackageNativeModules = False
        , elmPackageDependencies =
            Data.Map.fromList
                [ ( "elm-community/list-extra"
                  , Range
                        (Inclusive (version 5 0 0 [] []))
                        (Exclusive (version 6 0 0 [] []))
                  )
                , ( "elm-lang/core"
                  , Range
                        (Inclusive (version 5 0 0 [] []))
                        (Exclusive (version 6 0 0 [] []))
                  )
                ]
        , elmPackageElmVersion =
            Just $ Range
                (Inclusive (version 0 18 0 [] []))
                (Exclusive (version 0 19 0 [] []))
        }


typicalExampleJSON :: ByteString
typicalExampleJSON =
    encodeUtf8
        [text|
            {
                "version": "1.0.0",
                "summary": "Dict with arbitrary ordering (like List)",
                "repository": "https://github.com/Gizra/elm-dictlist.git",
                "license": "MIT",
                "source-directories": [
                    "src"
                ],
                "exposed-modules": [
                    "DictList"
                ],
                "dependencies": {
                    "elm-community/list-extra": "5.0.0 <= v < 6.0.0",
                    "elm-lang/core": "5.0.0 <= v < 6.0.0"
                },
                "elm-version": "0.18.0 <= v < 0.19.0"
            }
        |]
