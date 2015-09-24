Proper time handling can make your game look more realistic, 
support other hardware better, and optimise resource usage.

When it comes to time, it pays to understand the different notions of time in
your application. First, there's the user's (real) time. Then there's the CPU
time. SDL keeps its own clock, started when it is initialized. Your game may
also have a sense of time (independent from SDL's or the CPU's; the game clock
may be paused).

Knowing about these differences helps understand that we need to be able to
deal with the computer's time, and with one or more abstract notions of time
(eg. your game might show time-driven animations while the action is paused).
In the first part of this chapter we will deal with SDL's time facilities. In
the second part, we will deal with game aspects that are time-related, such as
pausing the game, basic physics and animating characters. See Game Programming
Concepts for more advanced and detailed uses of time.

# SDL's time

SDL provides a cross-platform clock with millisecond precision. The time API
only provides five functions: one to get the time, one to actively delay the
current thread, and three to install future call-backs.

Dealing with callbacks tends to make your code harder to understand, so we will
only introduce the main two functions.

## Detecting current time

Detecting the current time is essential to provide realistic animations and
physics. 

PLACE FOR A VISUAL EXAMPLE

SDL counts the number of milliseconds since the initialisation. The following example
should be straightforward to understand:

````haskell
import Control.Monad   (forever)
import Graphics.IO.SDL as SDL

main :: IO ()
main = do
  SDL.Init [InitAll]
  screen <- SDL.setVideoMode 480 320 32 [SWSurface]
  forever $ do
    let format = surfaceGetPixelFormat screen
    green <- SDL.mapRGB format 0 0xFF 0
    SDL.fillRect screen Nothing green
    SDL.flip screen

    -- NEW
    n <- SDL.getTicks
    putStrLn n
```

The first part of this example is the same in the first lesson. The second part illustrates
how to get the number of milliseconds (an Int).

A common calculation is to report the number of frames per second (or time per frame):

```haskell
import Control.Monad   (forever)
import Graphics.IO.SDL as SDL

main :: IO ()
main = do
  SDL.Init [InitAll]
  screen <- SDL.setVideoMode 480 320 32 [SWSurface]

  -- We need the start time, so that the first
  -- report is reliable (in case setting up SDL
  -- takes too long)
  lastTime <- getTicks
  render lastTime 0

render :: Int -> Int -> IO ()
render lastTime numFrames = do
    -- Do something expensive, like drawing on the screen
    let format = surfaceGetPixelFormat screen
    green <- SDL.mapRGB format 0 0xFF 0
    SDL.fillRect screen Nothing green
    SDL.flip screen

    -- NEW: report FPS every second
    newTime <- SDL.getTicks
    let timeNextReport = lastTime + 1000                       -- print one report per second
        timeDiff       = newTime - lastTime                    -- time passed since last report

        -- Time passed, in seconds
        timeDiffSecs :: Float
        timeDiffSecs = fromIntegral timeDiff / 1000

        -- Num frames in time passed
        fps :: Float
        fps = fromIntegral numFrames / timeDiffSecs

    -- Print report and start over,
    -- or continue looping and increase number of frames
    if newTime > timeNextReport
      then do putStrLn fps
              render newTime 0
      else render lastTime (numFrames + 1)
```
  
There is no strong reason behind using floats vs using a type with more
precision like a double, although the kinds of numbers that you are likely to
obtain will probably be relatively large and the error introduced by your own
counting operations may be too large for small differences to matter.

### Homework

* Modify the previous program to print the report every 100 frames

* Modify the previous program to print also the average time per frame

* Modify the previous program to print ***also*** the average FPS and time per
  frame since the beginning of the execution.

* The following program shows a circle going around in fixed steps. If you run
  it with a much faster CPU, the circle will move faster too. Modify the
  following program so that the circle takes one second per lap.

``` haskell

import Control.Monad
import Graphics.UI.SDL                as SDL
import Graphics.UI.SDL.Gfx.Primitives as SDL

main :: IO ()
main = do
  SDL.init [InitVideo]
  screen <- setVideoMode 480 320 32 [SWSurface]

  render 0.0

render :: Int -> IO ()
render percent = do

  -- Clear screen
  screen <- SDL.getVideoSurface
  let format = SDL.getSurfacePixelFormat screen
  green <- SDL.mapRGB format 0 0xFF 0

  -- Calculate new pos
  let angle = 2 * pi * (fromIntegral percent) / 100
      x = cos x * radius -- these use the rotation
      y = sin x * radius -- radius, not the circle's
  
  -- Draw circle
  SDL.filledCircle screen (fromIntegral x) (fromIntegral y) 30
                          (Pixel 0xFF0000FF) -- red

  if percent > 100
     then render 0
     else render (percent + 1)
```

## Delaying the game loop

Another useful time function is delaying. Delaying allows you to lower the
consumption of (a part of) your game and to wait until threads complete (for
instance, if you are playing music).

The following code should render the same animation we created above,
only slightly slower (notice we only introduce an artificial delay after
each frame is rendered):

``` haskell

import Control.Monad
import Graphics.UI.SDL                as SDL
import Graphics.UI.SDL.Gfx.Primitives as SDL

main :: IO ()
main = do
  SDL.init [InitVideo]
  screen <- setVideoMode 480 320 32 [SWSurface]

  render 0.0

render :: Int -> IO ()
render percent = do

  -- Clear screen
  screen <- SDL.getVideoSurface
  let format = SDL.getSurfacePixelFormat screen
  green <- SDL.mapRGB format 0 0xFF 0

  -- Calculate new pos
  let angle = 2 * pi * (fromIntegral percent) / 100
      x     = baseX + cos x * radius -- these use the rotation
      y     = baseY + sin x * radius -- radius, not the circle's
      baseX = 50
      baseY = 50
      radius = 50
  
  -- Draw circle
  SDL.filledCircle screen (fromIntegral x) (fromIntegral y) 30
                          (Pixel 0xFF0000FF) -- red

  -- 20 ms (we limit the FPS to a
  -- maximum of 100, probably less because drawing
  -- takes some time)
  SDL.delay 20

  if percent > 100
     then render 0
     else render (percent + 1)
```

Run the code again. Do you see the animation going slower?

### Homework

* Introduce a one second delay only after every lap. 

* Introduce a delay similar to the one above in the variation that you wrote
as part of the first section homework. Verify that your circle still completes
one lap per second (even if the animation is slightly jumpy).

* Modify the program above to also report the FPS.

* Modify the program above to adjust the delay to try to match a specific
number of frames per second.

# Time-dependent behaviours

## Pausing and handling different timelines

## Movement and physics

## Animation
