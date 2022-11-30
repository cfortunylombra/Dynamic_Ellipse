#include "pch.h"
#include "Sat2EarthLibrary.h"
#include <iostream>

// Definition: This library transforms satellite centered coordinates to earth centered coordinates
float* Sat2Earth(float** MatrixSat2EC, float PTSat[]) {
	// Initialization of earth centered coordinates
	float PTEarth[3];

	// Perform matrix multiplication
	for (int i = 0; i < 3; i++) {
		PTEarth[i] = 0.0f;
		for (int j = 0; j < 3; j++) {
			PTEarth[i] = PTEarth[i] + MatrixSat2EC[i][j] * PTSat[j];
		}
	}

	// Return: Earth centered coordinates
	return (float*)PTEarth;
}