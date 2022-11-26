# Mathematica Optics Simulator
Authors:
Zadia Hughes, Kim Lindquist, EJ Mercer

## Example

```
OpticRenderStatic[
    {4, 3},         (* The bounds of the simulation *)
    {BasicMirror[0.1,0.1,0,1.5], BasicMirror[1,0,0,1], BasicMirror[0,1.235,9 Pi / 16, 2]},         (* The elements of the simulation *)
    ExecLimit -> 1000,         (* Change the execution limit to 1000 steps *)
    Sources -> {{-2, 1, -3 Pi / 32, 0.02}},         (* The list of sources - currently just a single beam *)
    PointSize -> 0.01         (* The rendered point size *)
]
```

## Functions & Explanations 

- SimulPhoton

Parameters: A list of elements, a photon's position and velocity
Returns: The photon's new position and velocity

This is the simplest function of the rendering stack. This function takes a list of elements and a single photon, and does the following:
1. Check if the photon is within the radial bound (within 1 unscaled unit) of each element
2. If it is, run the check function of that element to see if the update functiterson should be applied to that photon
3. If it passes the check function, run the update function and update the photon's position/velocity to that location
4. Otherwise, if any of these checks do not match, advance the photon's position by its velocity and return it

The result of this function is that one run of this function simulates one 'step' of that photon's motion.


- SimulBeam

Parameters: A list of elements, simulation bounds, a photon's position and velocity, and a limit number of simulations
Returns: A list of that photon's positions

This function takes one photon's initial position, and repeatedly applies the SimulPhoton function to that photon until either the simulation limit is reached or the photon goes out of the bounds of the simulation. It returns a list containing the photon's position at each step of the iteration.


- OpticRenderStatic

Parameters: A list of elements, a list of sources, and a boundary configuration
Returns: A rendered image of that simulation, witsout animation

This function takes the parameters for a simulation and calls SimulBeam for each of the sources' photons, then it composites the resulting beam and the images of the structures into a single image, which is returned.


- OpticRenderAnimated

This function is the same as the OpticRenderStatic method, except it animates the resulting list from the SimulBeam call to produce an animation of how beams would propagate over time.


## Other Structures

### Elements

An element is a function that returns a specific object, which contains information about how to simulate that object. The function always accepts the position parameters and can optionally accept additional parameters depending on the object. It always returns an Associations object with the following keys:
- "position" -> This acts as a passthrough of the element's position information.
- "check" -> A function that accepts the position information of the particle ({x, y, xVel, yVel}, in that order) and returns True if that particle would be affected by this element in this simulation step, or false otherwise.
- "update" -> A function that accepts the same position information, and returns a list of the new particle's information, in the same order.
- "graphics" -> A list of graphics elements that represent this object.


**Important note:** All elements should be modelled as an element of radius 1 at the origin, facing up. For example, a basic mirror is modelled as a flat mirror from (-1, 0) to (1, 0). Any rotation or transformation __of the simulation__ is handled automatically by the simulation engine; however, it is the responsibility of the element to correctly transform the graphics returned. This is why the element position information is given to the element - the x coordinate, y coordinate, scale, and angle (in terms of radians, where horizontal facing up is 0 radians).


The following are provided as default elements:

- SimpleMirror (Element)
- SimpleLens (Element)
- ComplexMirror (Element)
- ComplexLens (Element)

### Sources

The sources will be implemented in a similar manner to the elements - receiving a x coordinate, y ccoordinate, and scale, as well as any additional parameters. The sources return a list of initial photon positions, rather than an associations object. Sources themselves are not rendered.


The following are provided as default sources:

- BeamSource (Source)
- PointSource (Source)
- CollinearSource (Source)