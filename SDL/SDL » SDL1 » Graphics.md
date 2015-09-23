SDL graphics can be subdivided in four subchapters: surfaces, image handling,
drawing, and image transformation.

SDL introduces a concept of surface, which is like a page, or a part of a page,
which may or may not have anything on it. Doing SDL animations from surfaces
works almost like doing a collage:

<img src="http://orig14.deviantart.net/78e3/f/2007/266/9/9/magazine_collage_by_lizlovespink.jpg" width="320">

You have once main surface (the window or the screen) and you start pasting
other surfaces on top of it, in different positions, with different
orientations.  Once you are done, your main surface will contain the combined
image to be presented to users.
