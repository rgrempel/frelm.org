import Control.Concurrent
import Control.Monad
import Prelude

main :: IO ()
main =
    forever $ do
        putStrLn "Hello Winnipeg!"
        threadDelay 1000000
