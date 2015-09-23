SDL graphics can be subdivided in four subchapters: surfaces, image handling,
drawing, and image transformation.

# Surfaces

SDL introduces a concept of surface, which is like a page, or a part of a page,
which may or may not have anything on it. Doing SDL animations from surfaces
works almost like doing a collage:

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

Footnotes

1. The difference between a file surface and a software surface is quite
arbitrary: it is just an area in memory that holds bytes. The difference only
lies in how those two areas are created.
