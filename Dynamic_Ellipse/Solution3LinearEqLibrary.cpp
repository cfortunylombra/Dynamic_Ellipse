#include "pch.h"
#include "Solution3LinearEqLibrary.h"
#include <iostream>

// Definition: This library computes the 3 unknowns from linear equations
Sol3LinEq_struct Sol3LinEq(float& X1, float& Y1, float& X2, float& Y2, float& X3, float& Y3) {
	float A = 0;
	float B = 0;
	float CK = 0;

	float D = X1 * X1 * Y2 * Y3 * (X3 * Y2 - X2 * Y3) + X2 * X2 * Y1 * Y3 * (X1 * Y3 - X3 * Y1) + X3 * X3 * Y1 * Y2 * (X2 * Y1 - X1 * Y2);

	A = (Y1 * Y1 * (X2 * Y2 - X3 * Y3) + Y2 * Y2 * (X3 * Y3 - X1 * Y1) + Y3 * Y3 * (X1 * Y1 - X2 * Y2)) / D;
	B = (X1 * X1 * (X3 * Y3 - X2 * Y2) + X2 * X2 * (X1 * Y1 - X3 * Y3) + X3 * X3 * (X2 * Y2 - X1 * Y1)) / D;
	CK = (X1 * X1 * (Y2 * Y2 - Y3 * Y3) + X2 * X2 * (Y3 * Y3 - Y1 * Y1) + X3 * X3 * (Y1 * Y1 - Y2 * Y2)) / D;

	// Return three parameters: A,B and CK
	Sol3LinEq_struct output = { A,B,CK };
	return output;
}