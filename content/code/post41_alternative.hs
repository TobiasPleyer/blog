import           Data.List
import           System.IO
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector  as V


mkTuple [w,x,y,z] = (toDouble w, toInt x, toInt y, toInt z) :: (Double, Int, Int, Int)
  where
    toInt = read . T.unpack
    toDouble = read . T.unpack
mkTuple _ = error "unexpected"

extractData pred bounds vec =
  let bounds' = V.ifilter (\i _ -> pred i) bounds
  in V.map (\(s,e) -> V.slice s (e-s) vec) bounds'

isValid (_,_,_,v) = v == 1

main = do
  lines <- (tail . T.lines) <$> TIO.readFile "post38_example.csv"
  let
    rows = [mkTuple (T.split (== ',') line) | line <- lines]
    valid_rows = [row | row <- rows, isValid row]
    (times,d1,d2,_) = unzip4 valid_rows
    time_diffs = zip [1..] (zipWith (-) (tail times) (init times))
    package_idxs = [0] ++ [fst x | x <- time_diffs, (snd x) > 1e-4]
    package_bounds = V.fromList $ zip (init package_idxs) (tail package_idxs)
    d1_vec = V.fromList d1
    d2_vec = V.fromList d2
    data1 = extractData even package_bounds d1_vec
    data2 = extractData odd package_bounds d2_vec
  putStrLn "Data1"
  print $ data1
  putStrLn "Data2"
  print $ data2
