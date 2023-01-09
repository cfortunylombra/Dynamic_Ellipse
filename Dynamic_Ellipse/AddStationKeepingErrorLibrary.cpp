#include "pch.h"
#include "AddStationKeepingErrorLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library takes point ptin and transforms it with respect to the station keeping error
float* SkError(float* PTIn, float& DifAng) {
	// Initialization
	float FAC = 0.0f;

	float TanSk = std::tan(DifAng);

	float X = PTIn[0];
	float Y = PTIn[1];
	float D = std::sqrt(X * X + Y * Y);

	if (D == 0) {
		FAC = 1.0f;
	}

	else {
		FAC = (1.0 + TanSk / D) / (1.0 - TanSk * D);
	}

	float* PTOut = new float[3];

	PTOut[0] = X * FAC;
	PTOut[1] = Y * FAC;
	PTOut[2] = 1.0f;

	return PTOut;
}