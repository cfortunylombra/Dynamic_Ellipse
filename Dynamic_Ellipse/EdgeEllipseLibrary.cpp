#include "pch.h"
#include "EdgeEllipseLibrary.h"
#include "Constants.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library determines edge of an ellipse with polygon approx. every 30 deg, counterclockwise from x axis
float** ElPoly(float& Alpha, float& Beta) {
	float** ElPts;
	ElPts = new float* [36];
	for (int i = 0; i < 36; i++) {
		ElPts[i] = new float[2];
	}

	for (int i = 0; i < 17; i++) {
		float Ta = std::tan(10.0f * (float(i - 9.0f)) / Rad);
		float Ta2 = Ta * Ta;

		ElPts[i][0] = 1.0f / std::sqrt(Alpha + Beta * Ta2);
		ElPts[i][1] = ElPts[i][0] * Ta;
		ElPts[i + 18][0] = -ElPts[i][0];
		ElPts[i + 18][1] = -ElPts[i][1];
	}

	ElPts[17][0] = 0.0f;
	ElPts[17][1] = 1.0f / std::sqrt(Beta);
	ElPts[35][0] = -ElPts[17][0];
	ElPts[35][1] = -ElPts[17][1];
	return ElPts;
}