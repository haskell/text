{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Data.Char
import Control.Monad (forM_)
import Data.Accessor ((^=))
import Data.Function
import Data.Maybe
import Data.List
import Debug.Trace
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk (renderableToWindow)
import System.Environment (getArgs)
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T

data Row = Row {
      rowName :: !Text
    , rowMean :: !Double
    , rowMeanLB :: !Double
    , rowMeanUB :: !Double
    , rowStdDev :: !Double
    , rowStdDevLB :: !Double
    , rowStdDevUB :: !Double
    } deriving (Show)

parseRow :: Text -> Row
parseRow = f . T.split ","
  where f [n,m,ml,mu,s,sl,su] = Row {
                                  rowName = n
                                , rowMean = r m
                                , rowMeanLB = r ml
                                , rowMeanUB = r mu
                                , rowStdDev = r s
                                , rowStdDevLB = r sl
                                , rowStdDevUB = r su
                                }
        r = read . T.unpack

readCSV :: FilePath -> IO [Row]
readCSV = fmap (map parseRow . tail . T.lines . decodeUtf8) . B.readFile

groupRows :: [Row] -> M.Map Text [Row]
groupRows = M.map (sortBy (compare `on` rowName)) . foldr f M.empty
    where f r m = let (p,s) = T.breakEnd "/" (rowName r)
                  in M.insertWith' (++) (T.init . T.tail $ p)
                     [r { rowName = T.init s}] m

main = do
  args <- getArgs
  groups <- mapM (fmap groupRows . readCSV) args
  let dd = M.unionsWith (++) . map (M.map (:[])) $ groups
  forM_ (M.toList dd) $ \(tdesc,rows) -> do
      let desc = T.unpack tdesc
      if False
       then renderableToWindow (renderMark desc rows) 400 160
       else renderableToPNGFile (renderMark desc rows) 400 160
                                (printf "time-%s.png" (map clean desc))
  where clean '/' = '-'
        clean c | isSpace c = '-'
                | otherwise = c

instance BarsPlotValue LogValue where
    barsReference = LogValue 1e-300
    barsAdd (LogValue a) (LogValue b) = LogValue (a * b)

renderMark :: String -> [[Row]] -> Renderable ()
renderMark desc rows
  | useLog    = toRenderable linLayout
  | otherwise = toRenderable logLayout
  where
    useLog = minimum (map minimum values) >= maximum (map maximum values) / 25
    values = transpose . map (map rowMean) $ rows
    keys   = map git (head rows)
        where git r = let n = T.unpack . rowName $ r
                      in maybe n id . flip lookup mappings $ n
    mappings = [("bl", "lazy BS"), ("bs", "strict BS"), ("l", "list"),
                ("tl", "lazy T"), ("ts", "strict T")]

    linLayout = layout1_title ^= "Timings for \"" ++ desc ++ "\""
              $ layout1_plots ^= [ Left (plotBars linBars) ]
              $ layout1_left_axis ^= linLeftAxis
              $ layout1_bottom_axis ^= bottomAxis
              $ defaultLayout1 :: Layout1 Double Double

    logLayout = layout1_title ^= "Timings for \"" ++ desc ++
                "\" (log scale)"
              $ layout1_plots ^= [ Left (plotBars logBars) ]
              $ layout1_left_axis ^= logLeftAxis
              $ layout1_bottom_axis ^= bottomAxis
              $ defaultLayout1 :: Layout1 Double LogValue

    logLeftAxis = laxis_generate ^= autoScaledLogAxis logSecAxis
                $ laxis_reverse ^= False
                $ defaultLayoutAxis

    linLeftAxis = laxis_generate ^= autoScaledAxis linSecAxis
                $ defaultLayoutAxis

    bottomAxis = laxis_generate ^= autoScaledAxis typeAxis
               $ defaultLayoutAxis

    linBars = plot_bars_values ^= (zip [0.5,1.5..] values)
            $ defaultPlotBars

    logBars = plot_bars_values ^= (zip [0.5,1.5..] . map (map LogValue) $ values)
            $ defaultPlotBars

    typeAxis = la_labelf ^= (ix keys . floor)
             $ la_nLabels ^= length keys
             $ defaultLinearAxis

    ix (x:xs) n | n <= 0    = x
                | otherwise = ix xs (n-1)
    ix [] _                 = ""

    linSecAxis = la_labelf ^= secs
               $ defaultLinearAxis

    logSecAxis = loga_labelf ^= (secs . fromLV)
               $ defaultLogAxis

    fromLV (LogValue v) = v

-- | Try to render meaningful time-axis labels.
--
-- /FIXME/: Trouble is, we need to know the range of times for this to
-- work properly, so that we don't accidentally display consecutive
-- values that appear identical (e.g. \"43 ms, 43 ms\").
secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1e9   = (k/1e9)  `with` "Gs"
    | k >= 1e6   = (k/1e6)  `with` "Ms"
    | k >= 1e4   = (k/1e3)  `with` "Ks"
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "µs"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.0f %s" t u
               | t >= 1e4  = printf "%.0f %s" t u
               | t >= 1e3  = printf "%.0f %s" t u
               | t >= 1e2  = printf "%.0f %s" t u
               | t >= 1e1  = printf "%.0f %s" t u
               | otherwise = printf "%.0f %s" t u
