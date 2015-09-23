SDL (Simple DirectMedia Layer) is a library that provides an abstraction
over hardware and facilitates writing multimedia.

SDL provides an abstraction to create drawing areas, draw and manipulate
pictures, paint images from files, play audio, communicate over the network,
and interact with input devices.

SDL has been used in professional games. The Android version of
the popular Angry Birds uses SDL.

There currently exist two main releases of SDL: SDL 1.2 and SDL 2.  In Haskell,
SDL 1.2 is fairly well supported, especially the basic packages for surface,
image and audio manipulation. There have been several attempts at creating
bindings for SDL2. At Keera Studios, we use these bindings for mobile
applications, but there is a more recent effort at providing a well-maintained
SDL2 library.

SDL 1.2 constitutes a great way to get introduced to game programming in
Haskell: it runs on all major platforms, it is easy to get started with, and it
presents most of the challenges that you will encounter also in other
libraries. SDL 1.2 does not present an over-simplified programming environment:
you can create commercial professional Haskell games using SDL1.2. 

We are going to learn a few game programming and SDL programming concepts. Pay
close attention, we will be coming back to these in later courses. In this part
we will learn about multimedia, input handling and some of the intricacies of
interacting with foreign libraries.  At the end of this course, you should have
written a fun and good-looking Haskell game using SDL 1.2.

# Table of Contents

* [A first example in SDL](SDL » SDL1 » Introduction)
* [Graphics in SDL](SDL » SDL1 » Graphics)
* Maybe: [Time in SDL](SDL » SDL1 » Time)
* [Input in SDL](SDL » SDL1 » Input)
* [Audio in SDL](SDL » SDL1 » Audio)
* Maybe: Networking?
* [Resources](SDL » SDL1 » Resources)
* [Summary](SDL » SDL1 » Summary)
* [A Game in SDL](SDL » SDL1 » Summary)
* Annex: Installation on different platforms
* Annex: Haskell quirks using SDL
