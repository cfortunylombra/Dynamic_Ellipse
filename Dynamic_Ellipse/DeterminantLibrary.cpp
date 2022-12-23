#include "pch.h"
#include "DeterminantLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library evaluates a determinant
Det_struct Det(float& X1, float& Y1, float& X2, float& Y2) {
	// Initialization
	float A = 0.0f;
	float B = 0.0f;
	float CK = 0.0f;

	// Compute the derminant
	float D = std::pow(X1 * Y2 - X2 * Y1, 2);

	if (D != 0) {
		float A = (Y1 * Y1 + Y2 * Y2) / D;
		float B = (X1 * X1 + X2 * X2) / D;
		float CK = -2 * (X1 * Y1 + X2 * Y2) / D;
	}

	// Return three parameters of the ellipse
	Det_struct output = { A, B, CK };
	return output;
}
