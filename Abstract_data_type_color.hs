data Color = RGB Float Float Float | CMYK [Float]
        deriving Show

rgb2cmyk :: Float -> Float -> Float -> Color
rgb2cmyk red green blue = rgb2cmyk_data (RGB red green blue)

-- rgb2cmyk_data is using to avoid complicated input
-- and to allow user to enter z.b. rgb2cmyk 255 255 255
-- instead rgb2cmyk (RGB 255 255 255)

rgb2cmyk_data :: Color -> Color
rgb2cmyk_data (RGB red green blue) = if red == 0 && green == 0 && blue == 0
        then CMYK [0,0,0,1]
        else CMYK [newColor red, newColor green, newColor blue, 1-w]
         where w = maximum [red/255, green/255, blue/255]
               newColor xColor = ((w - (xColor/255))/w)