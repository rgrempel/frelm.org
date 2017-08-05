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
    pure $
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
            (DocComment blocks):remaining -> go (Just blocks) accum remaining
            (Decl (A _ decl)):remaining ->
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
        Definition {} -> pure ()
        PortDefinition {} -> pure ()
        Fixity {} -> pure ()
        TypeAnnotation (ref, _) (_, type_) ->
            [whamlet|
                <code>
                    ^{viewRef ref} : ^{viewType type_}
            |]
        Datatype (Commented _ (UppercaseIdentifier ident, _) _) _ -> pure ()
        TypeAlias _ (Commented _ (UppercaseIdentifier ident, _) _) _ -> pure ()
        PortAnnotation (Commented _ (LowercaseIdentifier ident) _) _ _ ->
            pure ()

viewRef :: Ref -> Widget
viewRef ref =
    case ref of
        VarRef mods (LowercaseIdentifier ident) ->
            [whamlet|
                $forall (UppercaseIdentifier mod) <- mods
                    #{mod}.
                #{ident}
            |]
        TagRef mods (UppercaseIdentifier ident) ->
            [whamlet|
                $forall (UppercaseIdentifier mod) <- mods
                    #{mod}
                .#{ident}
            |]
        OpRef (SymbolIdentifier ident) ->
            [whamlet|
                (#{ident})
            |]

viewType :: Type -> Widget
viewType (A _ type_) =
    case type_ of
        UnitType _ -> [whamlet| () |]
        TypeVariable (LowercaseIdentifier ident) -> [whamlet| #{ident} |]
        TypeConstruction constructor args ->
            case constructor of
                NamedConstructor idents ->
                    let combinedConstructor =
                            intercalate "." $
                            fmap (\(UppercaseIdentifier ident) -> ident) idents
                    in [whamlet|
                            #{combinedConstructor}
                            $forall typeArg <- fmap snd args
                                ^{viewType typeArg}
                        |]
                TupleConstructor _ ->
                    [whamlet|
                        (
                        $forall typeArg <- fmap snd args
                            ^{viewType typeArg},
                        )
                    |]
        TypeParens (Commented _ t _) -> [whamlet| (^{viewType t}) |]
        TupleType {} -> [whamlet| #{tshow type_} |]
        RecordType {} -> [whamlet| #{tshow type_} |]
        FunctionType {} -> [whamlet| #{tshow type_} |]
