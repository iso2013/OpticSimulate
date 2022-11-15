# Mathematica Optics Simulator
Authors:
Zadia Hughes, Kim Lindquist, EJ Mercer

### Functions & Explanations

<br>

Optics[dimensions, elements, (options)]:
-   The main call. Takes a {width, height} for the simulation field and a list of optics elements.
-   Returns the rendered optics object.

Each element is defined as a function:
\<ElementName\>[]:
-   Returns an Associations object with the following properties:
    - bounds: An expression with x, y to check if the coordinates fall within the bounds of this object.
    - checkFunc: A function that checks if this element changes the object.
    - updateFunc: A function that takes the old coordinates of the object and the new ones.
    - graphics: A graphics element that looks like the object.

#### Internal Functions:

SimulPhoton[dimensions, elements, coords]:
-   Performs a single simulation of a single photon, beginning at coords - {x, y, xV, yV}


<br>

AnimatedOptics[dimensions, elements, (options)]:
-   Similar to Optics, but animated.
-   In addition to the options in Optics, has additional ones: