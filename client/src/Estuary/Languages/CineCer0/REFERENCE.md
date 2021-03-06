# Cinecer0 Reference

The CineCer0 mini-language (pronounced “sin–ay–ser-oh”) language allows video files to b e projected temporally and geometrically, targeting similar functionality to that of [CineVivo](https://github.com/essteban/CineVivo), again with an economical Haskell-like notation.

## Playing Videos

"video.extension" --videos play as a string <br />
"videoURL" --you can add the URL to play videos <br />
"" --empty state

## Image

setWidth [w] $ -- 1 = natural video width <br/>
setHeight [h] $ -- 1 = natural video height <br />
setSize [wh] $ <br /> --one parameter will affect both width and heigh proportionally
setPosX [x] $ -- from left (-1) to right 1 <br />
setPosY [y] $ -- from bottom (-1) to top 1 <br />
setCoord [x] [y] $ <br />
setOpacity [o] $ -- from 0 - 1 (no opacity) <br />
setBlur [bl] $ -- 0 = no blur (1++ = more) <br />
setBrightness [br] $ --  0-0.9 = less, 1++ = more <br />
setContrast [c] $ -- 0-0.9 = less, 1++ = more <br />
setGrayscale [g] $ -- 0 = no grayscale, 1 = full grayscale <br />
setSaturate [s] $ -- 1 = natural video saturation (1++ = more, 1-- =less) <br />
circleMask [m] $ -- 0 is no mask, 0-0.99 makes the mask appear from biggest to smallest (it grows/decrease from the centre)
circleMask' [m] [w] [h] $ -- similar to circleMask but with two more parameters that move the center of the circleMask
sqrMask [m] $ -- 0 is no mask, 0-0.99 makes the mask appear from biggest to smallest (it grows/decrease from the centre)
rectMask [t] [r] [b] [l] $ -- accepts four parameters: top right bottom left, which are the amount of reduction in each side

## ramp

(ramp [Cicles] [Initial_Value] [End_Value]) <br />

ramp can be use in style functions. Example <br />
width (ramp x y z) <br />

## quant

$ quant [Cicle_Multiplier] [Offset] <br />

quant function has two values. Cycle multiplier aligns the anchor time with multiples of the given value. Offset is a value from 0 to 1 that will shift the starting position of the quantisation <br />

quant can be use with any style functions with or without a ramp. Example: <br />
opacity (ramp x y z) $ quant x y <br />


## Time Functions

natural [shift] $ -- aligns the starting time of the video with the first beat of the first measure <br />

every [cycles] [shift] $ -- adjusts the length to a given number of cycles <br />

round [shift] $ -- adjusts the length to the nearest number of measures in Estuary's tempo. <br />

roundMetre [shift] $ -- adjusts the length to the nearest number of measures multiple of 2,4,8,16,etc. in order to maintain the video synchronised with Estuary's tempo. <br />

chop [startPos] [endPos] [cycles] [shift] $ -- plays the video from the starting position (0-1) to the end position (0-1) stretching or compressing the length to adjust it to the number of cycles provided as a parameter. <br />

chop' [startPos] [cycles] [shift] $ --  plays the video from the starting position (0-1) stretching or compressing its length to adjust it to the number of cycles provided as a parameter. <br />

chopSecs [startPos] [endPos] [cycles] [shift] $ -- plays the video from the starting position to the end position stretching or compressing its length to adjust it to the number of cycles provided as a parameter. This function does not have the start and end positions normalized from 0 to 1. <br />
