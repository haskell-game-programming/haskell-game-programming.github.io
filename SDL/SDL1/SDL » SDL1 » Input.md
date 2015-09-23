Input handing in SDL works by polling events from an event queue.
Events represent user actions on our focusable area, including
mouse interactions (clicks, moving, etc.), key action (pressing,
releasing, etc.).

Input events are obtained using the function `Graphics.UI.SDL.pollEvent`.  When
the queue is empty and there are no more events left the function returns the
value `NoEvent`.

Events represent changes in the input devices, not the state of the input
device itself. For instance, when the mouse is moved you will get a MouseMotion
event, but if it isn't, no event will tell you the mouse position.

NOTE: To remember the last known mouse position, your game input subsystem
should define its own `Controller` which holds the last known state of the
input controller. The Controller does not need to be low-level or match the
input device exactly, it can be abstract and adapted to your game. Know,
however, that the same input event or controller state may have completely
different interpretations during gameplay: for instance, a click on the screen
may be interpreted as a menu activation at the beginning, selecting an option
later on and firing a gun during the game. The controller will not be the last
layer of abstraction between your input subsystem and the game logic, but it
will be the first. The controller will also make it easier to abstract your game
from SDL-specifics, which will be useful if you later consider using a different
version of SDL or a completely different multimedia library.

# Input events

## Mouse events

SDL provides access to three kinds of mouse events: a mouse button being depressed,
a mouse button being released, and the mouse being moved. When both things happen,
you will receive two events (in no particular order).

Following the above recommendation, we are going to write a controller for a shooting
game in which holding the mouse button down means firing continuously.

```haskell

data Controller = Controller
 { gunFiring :: Bool
 , gunPos    :: (Int, Int)
 }

defaultController :: Controller
defaultController = Controller False (0,0)

-- SDL controller-updating function
updateController :: Controller -> IO Controller
updateController c = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return c
    SDL.MouseButtonUp   _ _ SDL.ButtonLeft -> updateController (c { gunFiring = False })
    SDL.MouseButtonDown _ _ SDL.ButtonLeft -> updateController (c { gunFiring = True  })
    SDL.MouseMotion     _ _ x y            -> updateController (c { gunPos    = (x,y) })
    _                                      -> updateController c  -- Discard any other event
```

The function `updateController` we have written takes care of polling the
status of the event queue as necessary and updating the know state of the input
device.  We can now use this function to write a program that will draw a blue
circle where the mouse is, and turn it red when the mouse is being depressed.

``` haskell
main :: IO ()
main = do
  SDL.init [InitVideo, InitInput]
  SDL.setVideoMode width height 32 [SWSurface]
  SDL.setCaption "Input test" ""

  gameLoop emptyController

width  = 480
height = 320

gameLoop :: Controller -> IO ()
gameLoop c = do
  -- Sense
  c' <- updateController c

  -- Advance game state: Nothing to do here
  
  -- Render
  render c'

  -- Loop
  gameLoop c'

render :: Controller -> IO ()
render controller = do
  screen <- getVideoSurface

  -- 1) Green background
  let format = SDL.surfaceGetPixelFormat screen
  green <- SDL.mapRGB format 0 0xFF 0
  SDL.fillRect screen Nothing green

  -- 2) Gun
  let (x,y) = gunPos c'
      color = if gunFiring c' then Pixel 0xFF0000FF -- red  (alpha 255)
                              else Pixel 0x0000FFFF -- blue (alpha 255)
  SDL.filledCircle surface (fromIntegral x) (fromIntegral y) 30 color
```

As you can see, our game loop calls `updateController`, but otherwise it is
completely input-agnostic and knows nothing about SDL events. You could even
use a completly different input mechanism and `gameLoop` would remain unchanged.
This kind of modularity and separation of concerns is extremely important in
software, and games are no exception. In real games, our input will most
likely be configurable, which will mean that there will even be an extra layer of
abstraction between the Controller and SDL.

## Keyboard events

Keyboard events are very similar to mouse button presses. Keyboard events
carry a `Keysym`, which contains information about the key being depressed,
the modifiers that depressed at the same time (Control, Shift, etc.) and
the character that the key should produce.

To understand how they work, we are going to change the program above to use
the keyboard to move the circle instead of using the mouse.  To do that, we
change only the function `updateController`, which will now update the
position. Note that, because using keys we might move out of the screen, we
make sure that the position is within screen boundaries. Note, however, that if
the window is resized, your screen boundaries may change and the input
subsystem should know. SDL notifies of window resizes using events, so it is
trivial to adapt the code in this case, but it may not always be like this in
all multimedia libraries.

``` haskell
-- SDL controller-updating function
updateController :: Controller -> IO Controller
updateController c = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return c

    -- Movement
    SDL.KeyDown {SDL.Keysym SDLK_LEFT  _ _) ->
      updateController (c { gunPos = withinScreenBoundaries ((-1, 0) ^+^ gunPos c)})
    SDL.KeyDown {SDL.Keysym SDLK_UP    _ _) ->
      updateController (c { gunPos = withinScreenBoundaries ((0, -1) ^+^ gunPos c)})
    SDL.KeyDown {SDL.Keysym SDLK_DOWN  _ _) ->
      updateController (c { gunPos = withinScreenBoundaries ((0,  1) ^+^ gunPos c)})
    SDL.KeyDown {SDL.Keysym SDLK_RIGHT _ _) ->
      updateController (c { gunPos = withinScreenBoundaries ((1,  0) ^+^ gunPos c)})

    -- Fire
    SDL.KeyDown {SDL.Keysym SDLK_SPACE _ _) -> updateController (c { gunClick = True})
    SDL.KeyUp   {SDL.Keysym SDLK_SPACE _ _) -> updateController (c { gunClick = False})

    -- Anything else
    _ ->
      updateController c  -- Discard any other event

(^+^) :: Num a => (a, a) -> (a, a) -> (a, a)
(^+^) (x1, y1) (x2, y2) = (x1 + y1, x2 + y2)

withinScreenBoundaries :: Controller -> Controller
withinScreenBoundaries c = c { gunPos = (x', y') }
 where x' = inRange (0, width)  (fst (gunPos c))
       y' = inRange (0, height) (snd (gunPos c))

inRange :: Ord a => (a,a) -> a -> a
inRange (mn, mx) v
 | v < mn    = mn
 | v > mx    = mx
 | otherwise = v
```

Note that, to simplify our code, we have created three auxiliary functions: one to
keep the controller position within screen boundaries, one to add vectors, and one
to return the closest number within a subrange.

If you try the code above, you will see that the program now indeed moves one
pixel every time a key is depressed, but it does not keep moving when the key
is held down. It is very slow, barely noticeable. To address this problem, the
controller needs to know when each button is depressed, and then we need to
our game logic updating function to transform the controller state into a game
state: 
``` haskell

data Controller = Controller
 { gunFiring :: Bool
 , gunLeft   :: Bool
 , gunRight  :: Bool
 , gunUp     :: Bool
 , gunDown   :: Bool
 }

defaultController :: Controller
defaultController = Controller False False False False False

```

Our controller sensing function is now a bit simpler, although more verbose:

``` haskell
-- SDL controller-updating function
updateController :: Controller -> IO Controller
updateController c = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return c
    -- Movement
    SDL.KeyDown {SDL.Keysym SDLK_LEFT  _ _) -> updateController (c { gunLeft  = True  })
    SDL.KeyUp   {SDL.Keysym SDLK_LEFT  _ _) -> updateController (c { gunLeft  = False })
    SDL.KeyDown {SDL.Keysym SDLK_UP    _ _) -> updateController (c { gunUp    = True  })
    SDL.KeyUp   {SDL.Keysym SDLK_UP    _ _) -> updateController (c { gunUp    = False })
    SDL.KeyDown {SDL.Keysym SDLK_DOWN  _ _) -> updateController (c { gunDown  = True  })
    SDL.KeyUp   {SDL.Keysym SDLK_DOWN  _ _) -> updateController (c { gunDown  = False })
    SDL.KeyDown {SDL.Keysym SDLK_RIGHT _ _) -> updateController (c { gunRight = True  })
    SDL.KeyUp   {SDL.Keysym SDLK_RIGHT _ _) -> updateController (c { gunRight = False })

    -- Fire
    SDL.KeyDown {SDL.Keysym SDLK_SPACE _ _) -> updateController (c { gunClick = True})
    SDL.KeyUp   {SDL.Keysym SDLK_SPACE _ _) -> updateController (c { gunClick = False})

    -- Anything else
    _ ->
      updateController c  -- Discard any other event
```

We now need a function to keep and update the game state, which we do as follows:
``` haskell
data GameState = GameState
  { gsGamePos  :: (Int, Int)
  , gsGameFire :: Bool
  }

updateGameLogic :: Controller -> GameState -> GameState
updateGameLogic c gs = gs'
 where gs' = gs { gsGameFire = gunFire c
                , gsGamePos  = withinScreenBoundaries (vtotal ^+^ gsGamePos gs)
                }

      -- Displacement caused by input controller state
      vtotal = vl ^+^ vr ^+^ vu ^+^ vd

      -- Displacement caused by controller in each direction
      vl = if gunLeft  c then (-1, 0) else (0, 0)
      vr = if gunRight c then (1,  0) else (0, 0)
      vu = if gunUp    c then (0, -1) else (0, 0)
      vd = if gunDown  c then (0,  1) else (0, 0)

-- Write this for the game state instead
withinScreenBoundaries :: GameState -> GameState
withinScreenBoundaries c = c { gsGamePos = (x', y') }
 where x' = inRange (0, width)  (fst (gsGamePos c))
       y' = inRange (0, height) (snd (gsGamePos c))
```

To make this work you would also need to adapt `gameLoop`, which would
now receive the initial game state when called from `main`, and pass the
new game state at every iteration:

``` haskell

main :: IO ()
main = do
  SDL.init [InitVideo, InitInput]
  SDL.setVideoMode width height 32 [SWSurface]
  SDL.setCaption "Input test" ""

  gameLoop emptyController (GameState (0,0) False)

gameLoop :: Controller -> GameState ->  IO ()
gameLoop c gs = do
  -- Sense
  c' <- updateController c

  -- Advance game state
  let gs' = updateGameLogic c gs
  
  -- Render
  render c'

  -- Loop
  gameLoop c' gs'
```
