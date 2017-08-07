{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | So, the basic ideas is that, so much as possible, we'll
-- leave the vendor/elm-format/parser code alone, to make it
-- easier to track upstream changes. So, we'll define our own
-- types here, tracking information we want to track, and do
-- a second parsing run over the AST that the elm-format code
-- generates.
module Data.ElmModule where

import AST.Declaration
import AST.Module
import AST.V0_16
import AST.Variable
import Cheapskate.Types
import qualified Data.Map.Strict as Map
import Import.App hiding (many)
import Parse.Helpers (iParse)
import Parse.Module (elmModule)
import Reporting.Annotation
import Text.Parsec.Error

data ElmModule = ElmModule
    { elmModuleDocs :: Maybe Blocks
    , elmModuleDocumented :: Map String (Blocks, Declaration)
    } deriving (Show)

parseModule :: Text -> Either ParseError ElmModule
parseModule source = do
    elmFormatModule <- iParse elmModule (unpack source)
    pure
        ElmModule
        { elmModuleDocs = removeLocation $ docs elmFormatModule
        , elmModuleDocumented = extractDocs $ body elmFormatModule
        }

removeLocation :: Located a -> a
removeLocation (A _ a) = a

extractDocs :: [Decl] -> Map String (Blocks, Declaration)
extractDocs = go Nothing Map.empty
  where
    go pendingComment accum decls =
        case decls of
            DocComment blocks:remaining -> go (Just blocks) accum remaining
            Decl (A _ decl):remaining ->
                let next =
                        case pendingComment of
                            Just comment ->
                                maybe
                                    accum
                                    (\key ->
                                         Map.insert key (comment, decl) accum) $
                                docKeyForDeclaration decl
                            Nothing -> accum
                in go Nothing next remaining
            _:remaining -> go pendingComment accum remaining
            [] -> accum

docKeyForDeclaration :: Declaration -> Maybe String
docKeyForDeclaration decl =
    case decl of
        Definition {} -> Nothing
        PortDefinition {} -> Nothing
        Fixity {} -> Nothing
        TypeAnnotation (ref, _) _ ->
            case ref of
                VarRef _ (LowercaseIdentifier ident) -> Just ident
                TagRef _ (UppercaseIdentifier ident) -> Just ident
                OpRef (SymbolIdentifier ident) -> Just ident
        Datatype (Commented _ (UppercaseIdentifier ident, _) _) _ -> Just ident
        TypeAlias _ (Commented _ (UppercaseIdentifier ident, _) _) _ ->
            Just ident
        PortAnnotation (Commented _ (LowercaseIdentifier ident) _) _ _ ->
            Just ident

viewDeclaration :: Declaration -> Widget
viewDeclaration decl =
    case decl of
        Definition {} ->
            [whamlet|
                <div .elm-declaration.elm-definition>
                    <pre>#{tshow decl}
            |]
        PortDefinition {} ->
            [whamlet|
                <div .elm-declaration.elm-port-definition>
                    <pre>#{tshow decl}
            |]
        Fixity {} ->
            [whamlet|
                <div .elm-declaration.elm-fixity>
                    <pre>#{tshow decl}
            |]
        TypeAnnotation (ref, _) (_, type_) ->
            [whamlet|$newline never
                <div .elm-declaration.elm-type-annotation>
                    ^{viewRef ref}
                    ^{viewType type_}
            |]
        Datatype (Commented _ (UppercaseIdentifier ident, args) _) (OpenCommentedList tags (_, ((UppercaseIdentifier lastTagIdent, lastTagArgs), _))) ->
            [whamlet|$newline never
                <div .elm-declaration.elm-datatype>
                    <div .elm-name-with-args>
                        <div .elm-uppercase-identifier>
                            #{ident}
                        $forall (_, LowercaseIdentifier arg) <- args
                            <div .elm-lowercase-identifier>
                                #{arg}
                    <div .elm-datatype-tags>
                        $forall Commented _ ((UppercaseIdentifier tagIdent, tagArgs), _) _ <- tags
                            <div .elm-name-with-args>
                                <div .elm-uppercase-identifier>
                                    #{tagIdent}
                                $forall (_, type1) <- tagArgs
                                    ^{viewType type1}
                        <div .elm-name-with-args>
                            <div .elm-uppercase-identifier>
                                #{lastTagIdent}
                            $forall (_, type1) <- lastTagArgs
                                ^{viewType type1}
        |]
        TypeAlias _ (Commented _ (UppercaseIdentifier ident, args) _) (_, type_) ->
            [whamlet|$newline never
                <div .elm-declaration.elm-type-alias>
                    <div .elm-name-with-args>
                        <div .elm-uppercase-identifier>
                            #{ident}
                        $forall (_, LowercaseIdentifier arg) <- args
                            <div .elm-lowercase-identifier>
                                #{arg}
                    ^{viewType type_}
            |]
        PortAnnotation (Commented _ (LowercaseIdentifier ident) _) _ _ ->
            [whamlet|$newline never
                <div .elm-declaration.elm-port-annotation>
                    <pre>#{tshow decl}
            |]

viewRef :: Ref -> Widget
viewRef ref =
    case ref of
        VarRef mods (LowercaseIdentifier ident) ->
            [whamlet|$newline never
                <div .elm-ref.elm-var-ref>
                    $forall (UppercaseIdentifier mod) <- mods
                        <div .elm-uppercase-identifier>
                            #{mod}
                    <div .elm-lowercase-identifier>
                        #{ident}
            |]
        TagRef mods (UppercaseIdentifier ident) ->
            [whamlet|$newline never
                <div .elm-ref.elm-tag-ref>
                    $forall (UppercaseIdentifier mod) <- mods
                        <div .elm-uppercase-identifier>
                            #{mod}
                    <div .elm-lowercase-identifier>
                        #{ident}
            |]
        OpRef (SymbolIdentifier ident) ->
            [whamlet|$newline never
                <div .elm-ref.elm-op-ref>
                    <div .elm-symbol-identifier>
                        #{ident}
            |]

viewType :: Type -> Widget
viewType (A _ type_) =
    case type_ of
        UnitType _ ->
            [whamlet|$newline never
                <div .elm-type.elm-unit-type>
            |]
        TypeVariable (LowercaseIdentifier ident) ->
            [whamlet|$newline never
                <div .elm-type.elm-type-variable>
                    #{ident}
            |]
        TypeConstruction constructor args ->
            case constructor of
                NamedConstructor idents ->
                    [whamlet|$newline never
                        <div .elm-type.elm-type-construction>
                            <div .elm-named-constructor>
                                $forall UppercaseIdentifier ident <- idents
                                    <div .elm-uppercase-identifier>
                                        #{ident}
                            $forall typeArg <- fmap snd args
                                ^{viewType typeArg}
                    |]
                TupleConstructor _ ->
                    [whamlet|$newline never
                        <div .elm-type.elm-type-construction>
                            <div .elm-tuple-constructor>
                                $forall typeArg <- fmap snd args
                                    ^{viewType typeArg}
                    |]
        TypeParens (Commented _ t _) ->
            [whamlet|$newline never
                <div .elm-type.elm-type-parens>
                    ^{viewType t}
            |]
        TupleType types ->
            [whamlet|$newline never
                <div .elm-type.elm-tuple-type>
                    $forall Commented _ (type1, _) _ <- types
                        ^{viewType type1}
            |]
        RecordType base fields _ _ ->
            [whamlet|$newline never
                <div .elm-type.elm-record-type>
                    $forall Commented _ (LowercaseIdentifier ident) _ <- base
                        <div .elm-record-base>
                            #{ident}
                    $forall (_, (_, (Pair (LowercaseIdentifier key, _) (_, value) _, _))) <- fields
                        <div .elm-record-pair>
                            <div .elm-lowercase-identifier>
                                #{key}
                            ^{viewType value}
            |]
        FunctionType (type1, _) r _ ->
            [whamlet|$newline never
                <div .elm-type.elm-function-type>
                    ^{viewType type1}
                    $forall (_, _, type2, _) <- r
                        ^{viewType type2}
            |]
