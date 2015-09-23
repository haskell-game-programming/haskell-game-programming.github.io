This chapter will give you a fair view of how SDL works, how to create a
game based on SDL, and how each future piece will fit in the big
picture.

SDL creates an abstraction to facilitate interactive multimedia. Like in most
multimedia libraries, some elements need to be initialized before calling
any other SDL functions.

The common pattern for every SDL interaction is that there is a global element
(a part of the screen, an input device, an audio channel) that we need to
create or gain access to, and then we can interact with it by passing specific
data or using the SDL API. Once the resource is no longer needed, it can be
freed.

With respect to external entities, we can either query them or affect them.
Let's see a basic example of an SDL application that creates a green window.
Many applications and games, especially simple ones, are structured in two
steps: initialization, and a game loop. The game loop detects changes (user
input, time passed, etc.), progresses the game, and paints the next image on
the screen.

``` haskell
import Graphics.UI.SDL

main :: IO ()
main = do
  -- Initialization
  SDL.init [InitVideo, InitInput]

  -- Configuration
  screen <- setVideoMode width height bpp [SWSurface]
  setCaption "Test" ""

  -- Loop forever
  gameloop 0

gameLoop :: Int -> IO ()
gameLoop n = do

    -- Detect user input
    spaceDown <- detectSpaceDown

    -- Progress game state
    let n' = if spaceDown then 0 else (n + 1) `mod` 768

    -- Present image
    render n'

    -- Loop
    gameLoop n'

-- Sense user input: consume all pending input, return True
-- if any event was spacebar down, False otherwise
detectSpaceDown :: IO ()
detectSpaceDown = do
  userEvent <- pollEvent
  case userEvent of
    NoEvent                                  -> return False
    KeyDown (Keysym { symKey = SDLK_SPACE }) -> detectSpaceDown >> return True
    _                                        -> detectSpaceDown

-- Paint screen according to program state
render :: Int -> IO ()
render state = do
    -- 1) Get screen handle
    screen <- getVideoSurface

    -- 2) Choose color to paint
    let format = surfaceGetPixelFormat screen
    green <- mapRGB format 0 0xFF 0
    red   <- mapRGB format 0xFF 0 0
    blue  <- mapRGB format 0    0 0xFF
    let color = case state `div` 256 of
                  0 -> green
                  1 -> red
                  2 -> blue

    -- 3) Fill screen in
    fillRect screen Nothing color

    -- 4) Present screen
    SDL.flip screen
```

You are not expected to understand that code, only to take a quick look and try
to follow it's logic. The important ideas to take away are:

* The SDL subsystems that you are going to use need to be initialized (function main)

* You may need to keep handles to different entities (screen, colors,
  images, events, audio chunks, etc.) to use them later on.

* SDL works at a relatively low level, and it's mostly imperative (IO). Even
  functions to obtain colors (mapRGB) are monadic.

* Many games are structured using a game loop (gameLoop), in which one
senses input, progresses the game state (possibly moving physics), and presents
the new state. Although this kind of loop has its drawbacks, it works well for
games with no physics (board games) or for very simple games.

# Summary

* SDL is a low-level API for multimedia over input hardware, graphics, sound, etc.
* SDL subsystems needs to be initialized before they can be used.
* The SDL API is monadic (it can affect the architecture of the rest of our program).
* Game loops may be appropriate for many kinds of (simple) games, and they work
by sensing, progressing and rendering.

# What next?

* In the [next chapter](), we will present the basics of SDL graphics and
  animation, and how to work with images and drawings.
