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

The question is then, why a fifth step? The answer is not immediately obvious:
but it has to do with the fact that your game will continue executing
immediately after you start playing the audio, and how Haskell manages memory.
To understand it fully, let's review the following program, which loads and
plays a WAVE file:

``` haskell
import qualified Graphics.UI.SDL.Mixer.General  as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import qualified Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types
import qualified Graphics.UI.SDL.Mixer.Samples  as SDL.Mixer.Samples

main : IO ()
main = do
  SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 4096
  SDL.Mixer.Channels.allocateChannels 16
  wave <- SDL.Mixer.Samples.loadWAV "file.wav"
  SDL.Mixer.Channels.playChannel (-1) wave 0
  return ()
```

Compile and run this example and you will see that nothing is played. The
program finishes immediately. To continue playing at least once, introduce an
artificial delay. However, not any kind of delay works. For instance, the following
program will likely play the file:

``` haskell
import           Control.Concurrent
import           Graphics.UI.SDL                as SDL
import qualified Graphics.UI.SDL.Mixer.General  as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import qualified Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types
import qualified Graphics.UI.SDL.Mixer.Samples  as SDL.Mixer.Samples

main : IO ()
main = do
  SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 4096
  SDL.Mixer.Channels.allocateChannels 16
  wave <- SDL.Mixer.Samples.loadWAV "file.wav"
  SDL.Mixer.Channels.playChannel (-1) wave 0
  threadDelay 10000000
  -- Try also using SDL.delay 1000
```

While the following program will probably ***not*** play anything:
``` haskell
import           Control.Concurrent
import           Control.Monad
import           Graphics.UI.SDL                as SDL
import qualified Graphics.UI.SDL.Mixer.General  as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import qualified Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types
import qualified Graphics.UI.SDL.Mixer.Samples  as SDL.Mixer.Samples

main : IO ()
main = do
  SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 4096
  SDL.Mixer.Channels.allocateChannels 16
  wave <- SDL.Mixer.Samples.loadWAV "file.wav"
  SDL.Mixer.Channels.playChannel (-1) wave 0
  
  -- Wait for a while (cpu-consuming loop)
  mapM_ (\_ -> return ()) [1..10000000]

  return ()
```

The reason has to do with garbage collection, and its true also
for other SDL structures like surfaces. The call to `playChannel`
is asynchronous: it does not wait until the sound has finished
playing, instead returning immediately.

The first example does not play anything because the program continues
to execute, reaches its last instruction very quickly, and the runtime
system frees all memory and stops the program before it plays anything.

The second program plays the sound because, by introducing a delay,
SDL is given a chance to play for a while. Depending on how long
playing the sound takes, you may hear more or less, but you should
at least hear something.

The last program does not play because... *because Haskell's Memory Manager
thinks that `wave`, which holds your wave file, is no longer being used once
`mapM_` starts executing (at least, not from Haskell), and it invokes a Foreign
Pointer Finalizer callback, which frees the chunk in C, preventing it from
being played. The idea is that Haskell frees memory that C will need, and SDL
cannot play the file because it's no longer in memory*.

The way to address this problem is to ensure that Haskell does not free that
structure (`wave`) until all SDL operations that depend on it have finished.

One way would be to keep the chunk (`wave`) in a structure that cannot
be freed long enough for the sound to play (maybe in your game's asset manager).
But, if you free that sound (because you unload it), it may be difficult to
know whether it is still being played or not.

A good approach is to use `Foreign.ForeignPtr.touchForeignPtr`. This
low-level function ensures that, at some point, Haskell knows that some
structure will be used (the function does not really do anything, but because
it receives your structure as argument and the compiler does not know that it's
not going to be used, it decides not to free it). Now the program should
continue running for a while and the sound is played completely:

``` haskell
import           Control.Concurrent
import           Control.Monad
import           Foreign.ForeignPtr
import           Graphics.UI.SDL                as SDL
import qualified Graphics.UI.SDL.Mixer.General  as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import qualified Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types
import qualified Graphics.UI.SDL.Mixer.Samples  as SDL.Mixer.Samples

main : IO ()
main = do
  SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 4096
  SDL.Mixer.Channels.allocateChannels 16
  wave <- SDL.Mixer.Samples.loadWAV "file.wav"
  SDL.Mixer.Channels.playChannel (-1) wave 0

  -- Wait for a while (cpu-consuming loop)             --
  mapM_ (\_ -> return ()) [1..10000000]                --
                                                       --
  touchForeignPtr wave                                 --

  return ()
```


If you want `playChannel` to still be asynchronous, one relatively
simple way is as follows:

``` haskell
import           Control.Concurrent
import           Control.Monad
import           Foreign.ForeignPtr
import           Graphics.UI.SDL                as SDL
import qualified Graphics.UI.SDL.Mixer.General  as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import qualified Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types
import qualified Graphics.UI.SDL.Mixer.Samples  as SDL.Mixer.Samples

main : IO ()
main = do
  SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 4096
  SDL.Mixer.Channels.allocateChannels 16

  playFor "file.wav" 1000
  
  return ()

playFor :: FilePath -> Int -> IO ()
playFor fp ms = void $ forkOS $ do
  wave <- SDL.Mixer.Samples.loadWAV fp
  SDL.Mixer.Channels.playChannel (-1) wave 0
  SDL.delay ms
  touchForeignPtr wave
```

You may prefer to use Haskell's `threadDelay` instead of SDL's threads.  Just
keep in mind that, if you use `threadDelay`, then the main program will not
wait for the thread that is playing the sound once it finishes. Otherwise,
the two implementations are virtually equivalent.

## Summary

* To play sound effects we need to initialise the sound subsystem,
configure the sound channels and the buffer size, load a file and play it
on a channel.

* Sound playing is **asynchronous**.

* To avoid unwanted interactions between C and Haskell structures, we need to
  make sure that SDL structures are not freed while they are in use. We can
keep them in Haskell values that cannot be garbage collected (due to our
program's implementation) or introduce artificial delays and use dummy
functions that use the desired structures (like `touchForeignPtr`) to make
Haskell's memory manager "think" that the values may still be used.

## Homework

* Change the implementation to wait a bit longer or a bit less.

* Try to load a file that does not use the same frequency specified
  in `openAudio`. What happens?

* The buffer size determines how much needs to be in the buffer
  for it to be played (4096 in the example above). Try decreasing its size to
  1024 and then to lower values. Do the same for values much higher than 4096.
  What happens?

* Create a program that plays the same sound twice with a minimal delay,
  so that the overlap for a little while.

* Using the program that plays two sounds with a small overlap, decrease the
  number of allocated channels to 1 and run it again. What happens?

* Write a program that plays the same sound (for 1 second) every 5 seconds.

* Use `getTicks` from the time lesson to measure the number of milliseconds
  that loading the WAV file takes. Think how to prevent that delay in games
  in which the same sound is being played over and over.

# Background music

To be completed.
