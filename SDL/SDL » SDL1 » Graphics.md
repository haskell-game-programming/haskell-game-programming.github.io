SDL graphics can be subdivided in four subchapters: surfaces, image handling,
drawing, and image transformation.

# Surfaces

SDL introduces a concept of surface, which is like a page, or a part of a page,
which may or may not have anything on it. Doing SDL animations from surfaces
works almost like doing a collage:

<img src="http://orig14.deviantart.net/78e3/f/2007/266/9/9/magazine_collage_by_lizlovespink.jpg" width="320">

You have one main surface (the window or the screen) and you start pasting
other surfaces on top of it at different positions and with different
orientations. Once you are done, your main surface will contain the combined
image to be presented to users.

There are three kinds of surfaces in SDL: the screen surface (that is, a
surface that will be used by the video driver to show whatever needs to be
shown), virtual surfaces (in-memory surfaces created from other surfaces, by
cropping or copying them), and file surfaces (in-memory surfaces created by
loading images from PNG, JPG, etc. files).

## The video surface

In the first chapter, we saw that the video surface needs to be initialized,
and that we can then start drawing or pasting other surfaces on it.  The
following program creates a window, obtains its surface, and repeatedly paints
it green.

``` haskell
import Graphics.UI.SDL

main :: IO ()
main = do
  -- Initialization
  SDL.init [InitVideo]

  -- Configuration
  screen <- SDL.setVideoMode width height bpp [SWSurface]
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

This program creates a new window whose background is green. There is
a few noticeable aspects:
* As stated in the previous chapter, to work with surfaces we needed
to initialize the video subsystem.
* We created the window by setting the video mode. We were also
able to change its caption.
* We needed a handle on the screen surface to paint it green.
We obtained it directly from SDL.setVideoMode, but we could
have obtained the same one later from SDL.getVideoSurface.
* We painted the screen green using a drawing primitive. Drawing
primitives will be explained later on.
* To actually render the surface on the screen, we need to flip it,
that is, to tell SDL that it's ready to be used. That is because
SDL implements something called double buffering, in which
a surface is drawn offline and presented all at once to avoid
flickering.

NOTE: In the good-old days, programmers would modify the area
of the video memory that the video output was reading to draw
directly. Video was rendered progressively, which mean that
the screen might be progressively showing drawing operations
as they took place. This also meant that one might be able to see intermediate
inconsistent visual states (part of the image coming from the old game state,
and another coming from a newer game state).  This all lead to inconsistencies
and unpleasant visual effects.

Computers started including more video memory than what was
really necessary to store one full-screen video image. Thus programmers could
draw a new image in a part of the video memory that was not being presented
yet, and later tell the driver to start showing a different part of the memory
instead.  This lead to a technique called double-buffering (one buffer to
render, another to draw). Newer games include also triple buffering, and
they allow us to synchronize buffer switching with the monitor refresh
rate (a technique called VSync). To appreciate the difference between
all of these, see the following video:

<a href="http://www.youtube.com/watch?feature=player_embedded&v=ekl9zR-T_6U"
target="_blank"><img src="http://img.youtube.com/vi/ekl9zR-T_6U/0.jpg"
alt="Double buffering, Triple buffering, and no vsync" width="240" height="180"
border="10" /></a>
