#include "pch.h"
#include "PointsCheckEllipseLibrary.h"
#include <iostream>

// Definition: This library determines whether all points are contained in the minimum ellipse having the smallest axes
PointsCheck_struct PointsCheck(const long& M, float& AMin, float& CoefAB, float& ArMin, float** PSOut) {
	float RaMin = AMin;
	float Area0 = 1000001.0f;
	float A0 = 0.0f;
	float B0 = 0.0f;
	float CK0 = 0.0f;

	for (int i = 0; i < M; i++) {
		float XL = PSOut[i][0];
		float YL = PSOut[i][1];

		if (CoefAB * (XL * XL + YL * YL) > 1) {
			// Four parameters are returned (A0, B0, CK0, Area0)
			PointsCheck_struct output = { A0, B0, CK0, Area0};
			return output;
		}
	}

	Area0 = ArMin;
	A0 = 1 / std::pow(RaMin, 2);
	B0 = A0;
	CK0 = 0.0f;

	// Four parameters are returned (A0, B0, CK0, Area0)
	PointsCheck_struct output = { A0, B0, CK0, Area0 };
	return output;
}