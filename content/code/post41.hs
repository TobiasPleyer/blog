import           System.IO
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Control.Monad.Trans.Cont
import qualified Data.Vector  as V


mkValues rows = return (V.fromList (map (mkTuple . (T.split (== ','))) rows))
  where
    mkTuple [w,x,y,z] = (toDouble w, toInt x, toInt y, toInt z) :: (Double, Int, Int, Int)
    mkTuple _ = error "unexpected"
    toInt = read . T.unpack
    toDouble = read . T.unpack

filterValid values = return $ V.filter (\(_,_,_,x) -> x == 1) values

mkTimeDiffs times = return $ V.indexed $ V.zipWith (-) t i
  where
    i = V.init times
    t = V.tail times

mkBounds tdiffs = do
  idxs <- return (
            V.cons 0 (
            V.map ((+1) . fst) (
            V.filter (\t -> snd t > 1e-4) tdiffs)))
  return (V.zip (V.init idxs) (V.tail idxs))

extractData pred bounds vec =
  let bounds' = V.ifilter (\i _ -> pred i) bounds
  in V.map (\(s,e) -> V.slice s (e-s) vec) bounds'

process rows = do
  values <- mkValues rows
  valid_values <- filterValid values
  let (times, data1_row, data2_row, _) = V.unzip4 valid_values
  tdiffs <- mkTimeDiffs times
  bounds <- mkBounds tdiffs
  let
    data1 = extractData even bounds data1_row
    data2 = extractData odd bounds data2_row
  return (data1,data2)

main = do
  rows <- (tail . T.lines) <$> TIO.readFile "post38_example.csv"
  let (data1,data2) = evalCont (process rows)
  putStrLn "Data1"
  print $ data1
  putStrLn "Data2"
  print $ data2
