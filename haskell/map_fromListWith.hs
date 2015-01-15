import qualified Data.Map as Map
phoneBookToMap :: Ord k => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs 

main = do
	let phoneBook = [("betty","555-2938") ,("betty","342-2492") ,("bonnie","452-2928") ,("patsy","493-2928")
				,("patsy","943-2929") ,("patsy","827-9162") ,("lucille","205-2928") ,("wendy","939-8282")
				,("penny","853-2492") ,("penny","555-2111") ]
	print $ Map.lookup "patsy" $ phoneBookToMap phoneBook  -- Just ["827-9162","943-2929","493-2928"]