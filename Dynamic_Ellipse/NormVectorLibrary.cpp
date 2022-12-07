#include "pch.h"
#include "NormVectorLibrary.h"
#include <iostream>

// Definition: This library normalizes vectors in satellite system to have a unit z-component
float* NormVector(float PTSat[]) {
	// Initialization of satellite centered coordinates normalized
	float *PTSatNormZ = new float[3];

	// Perform division with respect to the z-coordinate
	for (int i = 0; i < 3; i++) {
		PTSatNormZ[i] = PTSat[i] / PTSat[2];
	}

	// Return: Satellite centered coordinates normalized
	return PTSatNormZ;
}