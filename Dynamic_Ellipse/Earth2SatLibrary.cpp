#include "pch.h"
#include "Earth2SatLibrary.h"
#include <iostream>

// Definition: This library transforms earth centered coordinates to satellite centered coordinates
float* Earth2Sat(float** MatrixEC2Sat, float PTEarth[]) {
	// Initialization of satellite centered coordinates
	float *PTSat = new float[3];

	// Perform matrix multiplication (For-loop)
	for (int i = 0; i < 3; i++) {
		PTSat[i] = 0.0f;
		// For-loop
		for (int j = 0; j < 3; j++) {
			PTSat[i] = PTSat[i] + MatrixEC2Sat[i][j] * PTEarth[j];
		}
	}

	// Return: Satellite centered coordinates
	return PTSat;
}