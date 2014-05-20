# pljelixir

This project contains the Elixir presentation that I did for the Montgomery
County Programming Language Junkies on May 19th, 2014. It uses the Nerves
project to create an SDCard image for the Raspberry Pi that shows off a
reveal.js presentation with support for live coding. An Erlang port process that
runs Qt is launched behind the scenes to run the web browser and expose IEx.

Important keys are:

* F1: toggle between presentation view and editor view
* F2: make the editor's font larger
* F3: make the editor's font smaller
* F5: reload the current webpage - useful for recovering from lost focus since
  I'm not using a mouse to click on the webpage again

## Building

Clone the repository and run make. It downloads Nerves and everything else
needed to create the SDCard image. If all goes right, the image will be the file
`_images/pljelixir.img`. Use `dd` or `mmccopy` to write it to an SDCard.
