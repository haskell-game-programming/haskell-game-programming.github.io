SDL distinguishes between two kinds of audio resources: background music and
(foreground) sound effects. There is only one background music; it is loaded
from an MP3 file, and it plays continuously.  Foreground music, on the other
hand, is loaded in chunks which are put in a channel that the audio driver
reads. Sound effects are loaded from WAVE files.

In this chapter we are going to see how to load, play, pause and stop different
kinds of music. We will also see how to play it continuously, and how to change
the volume. Finally, we will demonstrate one of the quirks of using Haskell and
a C API combined, namely that, without proper care, Haskell's Garbage Collector
will free our music from memory before it is played, so we will need to make
sure that it sticks around long enough.

NOTE: I barely remember the graphics of many games I played, but I can still
sing their tunes. Audio in games can enchance gameplay, introduce tension and
even make a fun game more fun. Music can make your games memorable or, if used
incorrectly, also annoying. Music and sound effects are a central part of your
games, do not underestimate their importance and dedicate a fair amount of time
to improving your game sound. What sounds cool to your friends when they hear
it once may not be so pleasant to players who play your game for hours. Beta
test your music.

# Foreground music

Foreground music works in five steps:
* Initialize the audio subsystem (once).
* Open a channel to play music.
* Load your audio file.
* Start playing the audio.
* Keep playing until the sound is over (introduce a delay).

The question is then, why a fifth step? Your game will continue executing
immediately after you start playing the audio. 

# Background music
