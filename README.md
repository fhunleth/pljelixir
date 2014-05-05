# pljelixir

This project contains the Elixir presentation that I did for the Montgomery
County Programming Language Junkies on May 19th, 2014. It uses the Nerves
project to create an SDCard image for the Raspberry Pi that shows off a
reveal.js presentation with support for live coding. An Erlang port process that
runs Qt is launched behind the scenes to run the web browser and code editor.

Important keys are:

* F1: toggle between presentation view and editor view
* F2: make the editor's font larger
* F3: make the editor's font smaller
* F5: reload the current webpage - useful for recovering from lost focus since
  I'm not using a mouse to click on the webpage again
* Enter: go to the next line
* Ctrl-Enter: submit the lines to Elixir
* Ctrl-C: get a fresh prompt
