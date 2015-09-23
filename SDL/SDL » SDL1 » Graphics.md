SDL graphics can be subdivided in four subchapters: surfaces, image handling,
drawing, and image transformation.

# Surfaces

SDL introduces a concept of surface, which is like a page, or a part of a page,
which may or may not have anything on it. Doing SDL animations from surfaces
works almost like doing a collage<sup>[1]</sup>:

<p align="center">
<img src="http://orig14.deviantart.net/78e3/f/2007/266/9/9/magazine_collage_by_lizlovespink.jpg" width="320">
</p>

There is one main surface (the window or the screen) onto which other surfaces
can be pasted at different positions and with different orientations. In the
end, the main surface will contain the combined image to be presented to users.

There are three kinds of surfaces in SDL: the screen surface (that is, a
surface that will be used by the video driver to show whatever needs to be
shown), file surfaces (in-memory surfaces created by loading images from PNG,
JPG, etc. files), and virtual surfaces (in-memory surfaces created from other
surfaces, by cropping or copying them)<sup>**1**</sup>.

## The video surface

In the first chapter we saw that the video surface needs to be initialized,
and that we can then start drawing or pasting other surfaces on it. The
following program creates a window, obtains its surface, and repeatedly paints
it green.

``` haskell
import Control.Monad   (forever)
import Graphics.UI.SDL as SDL

main :: IO ()
main = do
  -- Initialization
  SDL.init [InitVideo]

  -- Configuration
  screen <- SDL.setVideoMode 480 320 32 [SWSurface]
  SDL.setCaption "Test" ""

  forever $ do
    -- You can also do this here to get the main video surface
    -- screen <- SDL.getVideoSurface

    let format = SDL.surfaceGetPixelFormat screen
    green <- SDL.mapRGB format 0 0xFF 0
    SDL.fillRect screen Nothing green

    -- Double buffering
    SDL.flip screen
```

There is a few noticeable aspects:

* As stated before, to work with surfaces we needed
to initialize the video subsystem (SDL.init).

* We create the window by setting the video mode (size, bits per pixel,
  rendering options). We were also able to change the window's title (caption).

* We needed a handle on the screen surface to paint it.
We obtained it directly from `SDL.setVideoMode`, but we could
have obtained the same one later from SDL.getVideoSurface.

* We painted the screen green using a drawing primitive. Drawing
primitives will be explained later on.

* To actually render the surface on the screen, we need to flip it,
that is, to tell SDL that it's ready to be used. That is because
SDL implements something called double buffering, in which
a surface is drawn offline and presented all at once to avoid
flickering.

---

NOTE: In the old days, programmers would directly modify the area of the video
memory that the driver was reading from. As drawing operations took some time
to complete, the screen would show them as they took place, pixel by pixel,
which were inconsistent and visually unpleasant.

Newer computers included more video memory than
necessary to store one full-screen video image. Programmers could
then draw a new image in a part of the video memory that was not being
presented, and later tell the driver to start showing a different part of the
memory instead. This is called double buffering (one buffer
for rendering, another for drawing). Newer games feature triple buffering, and
also allow us to synchronize buffer switching with the monitor refresh rate
(VSync). To appreciate the difference between all of these, see the following
video:

<p align="center">
<a href="http://www.youtube.com/watch?feature=player_embedded&v=ekl9zR-T_6U"
target="_blank"><img src="http://img.youtube.com/vi/ekl9zR-T_6U/0.jpg"
alt="Double buffering, Triple buffering, and no vsync" width="240" height="180"
border="10" /></a>
</p>

---

### Exercises

* Change the program so that the screen is drawn red.

* Change the program so that the screen is drawn only the first time, then loop
 forever doing nothing (`return ()`). What happens then?

* Change the program so that the screen is drawn once, but do not flip the
video surface (double buffering) and then loop forever doing nothing. What
happens then?

* The second parameter to SDL.fillRect is a `Maybe SDL.Rect`. If it is
  `Nothing`, the whole surface is filled with the given color. If it is `Just`
  a `Rect`, then only the area specified by the rectangle is filled. `Rect` has
  four arguments: the first two are the coordinates of the rectangle's corner, and
  the other two are its size.  Change the program to draw a red rectangle in the
  middle of a completely green surface.

   * Are the coordinates of the rectangle specified from the center of the video
     surface, from the top-left corner, from the bottom-left corner, or from some
     other position?
  
   * What happens if you paint an area that is bigger than the surface, or partially
     out of it?

## Loading and pasting files

Loading image files could not be easier in SDL. The function `SDL.Image.load`
from the package `SDL-image` does all the work for us. It guesses the format
based on the extension, and gives us a surface that contains the image already.
We can use that handle to paste the image onto the video surface. Note that
we need to obtain the surface size and specify the position on which we want
to paste the image.

``` haskell
import Control.Monad         (forever)
import Graphics.UI.SDL       as SDL
import Graphics.UI.SDL.Image as SDL

main :: IO ()
main = do
  -- Initialization
  SDL.init [InitVideo]

  -- NEW: load image
  image <- SDL.load "bunny1_jump.png"
  let imgWidth  = surfaceWidth  image
      imgHeight = surfaceHeight image

  -- Configuration
  screen <- SDL.setVideoMode 480 320 32 [SWSurface]
  SDL.setCaption "Test" ""

  forever $ do
    -- You can also do this here to get the main video surface
    -- screen <- SDL.getVideoSurface

    let format = SDL.surfaceGetPixelFormat screen
    green <- SDL.mapRGB format 0 0xFF 0
    SDL.fillRect screen Nothing green

    -- NEW: Paste the image surface onto the gree background
    SDL.blitSurface screen Nothing image (Just (Rect 30 30 imgWidth imgHeight))

    -- Double buffering
    SDL.flip screen
```

### Homework

* Copy a bunny onto each corner of the video surface. Change the default size
of 480x320 pixels and check that the bunnies are still being shown in the corners
of the screen.

* What happens if you resize the window?

* Load two images instead of one. Move the rendering code in the do block to a
separate function that receives both images as a parameter, and has a third
`Int` parameter. Make that function call itself after rendering each frame,
increasing the value in one unit every time, up to 512 and then going back to 0.
Make the function blit the first image if the argument is lower than 256,
and the second if the argument is between 256 and 512.

### Notes

* We have not seen how to handle time yet. This way of producing animations
is suboptimal because the speed of the animation depends on CPU speed and load.
The animation might speed up or slow down during gameplay, and it might run
too fast in next-generation computers (something that happened to most
80386 games when Pentiums came around).

## Software surfaces

Finally, we are going to see how to create a completely in-memory surface.
These are useful when we need to pass a surface to an operation (for instance,
as a white canvas to draw onto), and then blit that surface onto a larger
surface at a given position.

For instance, in a game, you might want to draw the game scene on a virtual
surface, the game menu on another surface, and then blit them both in specific
coordinates of the main video surface. This way, none of the sub-rendering
functions needs to know about its position with respect to the parent surface.

<!-- Could it be clearer with a drawing, or a code comparison? -->

The following code creates two surfaces, draws two subparts of the UI in them,
pastes them both in the main video surface, and then presents the main surface.

### Homework

* Create the following UI using the assets from TOBECOMPLETED
and the code template from TOBECOMPLETED.

### Notes

* While it makes your code clearer, using software surfaces may be expensive.
  If you abuse them, your code will run subtantially slower.

# Drawing arbitrary shapes

SDL allows you to draw primitive shapes onto the screen. Unlike surfaces, shapes
are drawn directly without blitting them. The current Haskell bindings for SDL-gfx
are less developed than the main project's bindings and use Word-based types.
Nevertheless, they achieve their purpose. Let's draw a few figures on the screen.

PLACE FOR A CODE EXAMPLE

Drawing these shapes may be more tedious than using PNG files. While you can
achieve aesthetically-pleasant effects and your UIs will be easily scalable if
you draw shapes by hand using SDL-gfx, your game make take longer to develop.
If you are not an experienced graphics programmer and designer, programming
quality graphics may proof too difficult in the end. That is why most
programmers use assets created by graphics designers in some graphic format.

Always keep in mind that the end goal is to complete a fun game, and that
nobody will care if your game was prefectly scalable if you never released it.

### Homework

To be completed

# Transforming images

To complete this chapter on SDL graphics, we are going to see how to manipulate
images. You can perform a few transformations, namely rotations, flipping, etc.

The following code example should make it clear how these transformations
work.

TO BE COMPLETED

## Footnotes

1. The difference between a file surface and a software surface is quite
arbitrary: it is just an area in memory that holds bytes. The difference only
lies in how those two areas are created.

## References

* [1] Image created by [lizlovespink](http://lizlovespink.deviantart.com/art/Magazine-Collage-65621965).
