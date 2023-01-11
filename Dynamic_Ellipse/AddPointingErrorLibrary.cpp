#include "pch.h"
#include "AddPointingErrorLibrary.h"
#include "Constants.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library takes point ptin and transforms it with respect to the pointing error
float** PtError(float* PTIn, float& TanPe) {
	float X = PTIn[0];
	float Y = PTIn[1];
	float D = std::sqrt(X * X + Y * Y);

	float FAC = (1.0f + TanPe / D) / (1.0f - TanPe * D);

	float XX = X * FAC; 
	float YY = Y * FAC;
	float DelX = XX - X;
	float DelY = YY - Y;

	float** PT7Out;
	PT7Out = new float* [7];
	for (int i = 0; i < 7; i++) {
		PT7Out[i] = new float[3];
	}

	for (int i = 0; i < 7; i++) {
		float Ang = (-120 + i * 30) / Rad;
		PT7Out[i][0] = X + DelX * std::cos(Ang) - DelY * std::sin(Ang);
		PT7Out[i][1] = Y + DelY * std::cos(Ang) - DelX * std::sin(Ang);
		PT7Out[i][2] = 1.0f;
	}

	return PT7Out;
}