{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings.StaticFiles where

import Settings.App (appStaticDir, compileTimeAppSettings)
import Yesod.Static (staticFilesList)

-- This generates easy references to files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--
--     js_script_js
--
-- If the identifier is not available, you may use:
--
--     StaticFile ["js", "script.js"] []
staticFilesList (appStaticDir compileTimeAppSettings)
    [ "bower_components/jquery/dist/jquery.min.js"
    , "bower_components/bootstrap/dist/css/bootstrap.min.css"
    , "bower_components/bootstrap/dist/css/bootstrap-theme.min.css"
    , "bower_components/bootstrap/dist/js/bootstrap.min.js"
    , "bower_components/bootstrap/dist/fonts/glyphicons-halflings-regular.eot"
    , "bower_components/bootstrap/dist/fonts/glyphicons-halflings-regular.svg" 
    , "bower_components/bootstrap/dist/fonts/glyphicons-halflings-regular.ttf"
    , "bower_components/bootstrap/dist/fonts/glyphicons-halflings-regular.woff"
    , "bower_components/bootstrap/dist/fonts/glyphicons-halflings-regular.woff2"
    ]
