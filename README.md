## ITU Project

GIMS is a standalone application used in the Radiocommunication Bureau of the ITU to capture, store, analyze, browse etc graphical data related to satellite networks.

One of the functionalities in GIMS is the calculation of the optimal ellipse which encompasses a set of test points. This mathematical calculation is applied in determining the elliptical beam of a satellite antenna by generating gain contours covering a required service area defined by a set of earth station locations.

This functionality is currently implemented in ellebu.dll. Ellebu.dll is called from Visual C++ code in GIMS. The dll was developed in FORTRAN, in the 90s. In the context of the modernization of the legacy code this code needs to be rewritten. We could take the chance to improve it.

The goal is to produce a new version of this dll. This new version must have similar interface and the same functionality. Not necessarily the same exact results as the current dll are necessary, but the differences in the results must be justified.

