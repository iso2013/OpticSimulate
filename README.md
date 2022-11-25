# Mathematica Optics Simulator
Authors:
Zadia Hughes, Kim Lindquist, EJ Mercer

### Functions & Explanations 

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
Returns: A rendered image of that simulation, without animation

This function takes the parameters for a simulation and calls SimulBeam for each of the sources' photons, then it composites the resulting beam and the images of the structures into a single image, which is returned.


- OpticRenderAnimated

This function is the same as the OpticRenderStatic method, except it animates the resulting list from the SimulBeam call to produce an animation of how beams would propagate over time.


### Other Structures

- SimpleMirror (Element)
- SimpleLens (Element)
- ComplexMirror (Element)
- ComplexLens (Element)


- BeamSource (Source)
- PointSource (Source)
- CollinearSource (Source)