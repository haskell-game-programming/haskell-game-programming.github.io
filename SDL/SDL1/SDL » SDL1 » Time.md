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

SDL provides a cross-platform clock with millisecond precision.  The timer API
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

    -- NEW
    n <- SDL.getTicks
    putStrLn n
```

The first part of this example is the same in the first lesson. The second part illustrates
how to get the number of milliseconds (an Int).

A common calculation is to report the number of frames per second (or time per frame):
````haskell
import Control.Monad   (forever)
import Graphics.IO.SDL as SDL

main :: IO ()
main = do
  SDL.Init [InitAll]
  screen <- SDL.setVideoMode 480 320 32 [SWSurface]

  n <- getTicks
  render n 0

render :: Int -> Int -> IO ()
render lastTime numFrames = do
    let format = surfaceGetPixelFormat screen
    green <- SDL.mapRGB format 0 0xFF 0
    SDL.fillRect screen Nothing green

    -- NEW: report FPS every second
    newTime <- SDL.getTicks
    if newTime > lastTime + 1000
      then do putStrLn (fromIntegral (newTime - lastTime)) / fromIntegral numFrames)
              render newTime 0
      else render lastTime (numFrames + 1)
```
  

## Delaying the game loop

### Aiming for a constant framerate

# Time-dependent behaviours

## Pausing and handling different timelines

## Movement and physics

## Animation
