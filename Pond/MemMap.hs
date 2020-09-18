import Data.Map

main :: IO ()
main = print $ Data.Map.lookup "epic" $ insert "epic" 69 $ fromList [("a", 3), ("b", 8)]