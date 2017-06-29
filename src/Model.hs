{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Data.PersistSemVer ()
import Data.SemVer (Version)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
    [mkPersist sqlSettings]
    [persistLowerCase|
        User sql=users
            name Text
            email Text
            avatarUrl Text Maybe
            plugin Text
            ident Text

            UniqueUser plugin ident
            deriving Eq Show Typeable

        Repo sql=repos
            gitUrl Text
            submittedBy UserId Maybe

            UniqueRepo gitUrl
            deriving Eq Show

        RepoVersion
            repo RepoId
            tag Text
            version Version
            package Text
            -- TDOD: Use a JSON type?

            UniqueRepoVersion repo version
            deriving Eq Show
    |]
