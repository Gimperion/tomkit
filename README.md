# tomkit R package.
by @gimperion
=====================

Utility functions for Gimperion's code.

Newly added:  pipeMarker(x, text) helps allows insertion of messages along a piped sequence (dplyr, magrittr, etc.).  Useful for computationally intensive pipes.

myFunction(x) %>%
    pipeMarker("Going onto the next stage!") %>%
    myNextFunction()
