{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.ElmPackageSpec (spec) where


import Data.Aeson
import Data.ElmPackage
import Data.SemVer (Version, version)
import NeatInterpolation (text)
import TestImport


spec :: Spec
spec = withApp $ do
    describe "FromJSON" $ do
        it "parses a typical example" $ do
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
        , elmPackageDependencies =
            [ Dependency
                { label =
                    "elm-community/list-extra"
                , bounds =
                    VersionBounds
                        (version 5 0 0 [] [])
                        (version 6 0 0 [] [])
                }
            , Dependency
                { label =
                    "elm-lang/core"
                , bounds =
                    VersionBounds
                        (version 5 0 0 [] [])
                        (version 6 0 0 [] [])
                }
            ]
        , elmPackageElmVersion =
            VersionBounds
                (version 0 18 0 [] [])
                (version 0 19 0 [] [])
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
