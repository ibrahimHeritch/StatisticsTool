import Codec.Picture
import Data.Set (Set)
import qualified Data.Set as Set
import MinMax
scatterPlotPixelMapper yScale xScale points l x y
    | x2 < 0 = PixelRGB8 255 255 255
    | y2 < 0 = PixelRGB8 255 255 255
    | x2 > 280 = PixelRGB8 255 255 255
    | y2 > 280 = PixelRGB8 255 255 255
    | x2 == 0  = PixelRGB8 0 0 0
    | x2 == 280 = PixelRGB8 0 0 0
    | y2 == 0  = PixelRGB8 0 0 0
    | y2 == 280 = PixelRGB8 0 0 0
    | Set.member ((x1 `div` 7) * 7 ,(y1 `div` 7) * 7) s = PixelRGB8 186 33 9
    | l x1 y1 =  PixelRGB8 28 189 13
    | otherwise = PixelRGB8 172 224 232
    where
      x1 = (xScale (x))
      y1 = (yScale y)
      x2 = x1 `div` 3
      y2 = y1 `div` 3
      s = Set.fromList(map (\(x , y)-> ((x `div` 7) * 7,(y `div` 7) * 7)) points)
line a b x y
    | x>=y = y == roundedY || y == roundedY - 1 || y == roundedY + 1
    | x<y = x ==  roundedX ||  x == roundedX - 1 || x == roundedX + 1
    where
      roundedY = round((fromIntegral(x)*a) + b)
      roundedX = round((fromIntegral(y)/a) - b)



testSP = writePng "output.png" ( generateImage (scatterPlotPixelMapper ((\y ->870-(y - 30)))((\x ->(x - 30)))(( [(0 ,20),(22,22),(66,67),(54,55),(75,79),(80,67),(100, 78)]))(line 0.9 20)) 900 900)
