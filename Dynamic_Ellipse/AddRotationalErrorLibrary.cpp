#include "pch.h"
#include "AddRotationalErrorLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library takes point ptin and transforms it with respect to the rotation error
float** RotError(float* PTIn, float& CROT, float& SROT) {

	float X0 = PTIn[0];
	float Y0 = PTIn[1];

	float** PTSOut;
	PTSOut = new float* [3];
	for (int i = 0; i < 3; i++) {
		PTSOut[i] = new float[3];
	}

	PTSOut[0][0] = X0;
	PTSOut[0][1] = Y0;
	PTSOut[0][2] = 1.0;

	PTSOut[1][0] = X0*CROT - Y0*SROT;
	PTSOut[1][1] = X0*SROT + Y0*CROT;
	PTSOut[1][2] = 1.0;

	PTSOut[2][0] = X0 * CROT + Y0 * SROT;
	PTSOut[2][1] = -X0 * SROT + Y0 * CROT;
	PTSOut[2][2] = 1.0;

	return PTSOut;
}