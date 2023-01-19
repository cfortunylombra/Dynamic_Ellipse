#include "pch.h"
#include "MinimumSemiMinorAxisLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library determines an ellipse with a given minimum semi-major axis, having the smallest area
MinAxis1_struct MinAxis1(const long& M, float& A0, float& B0, float& CK0, float& AMin, float** PSOut) {
	float RaMin = AMin;
	float Beta = 1.0f / std::pow(RaMin, 2);
	float Area0 = 1000001.0f;

	main_forloop:
		for (int i = 0; i < M; i++) {
			float XL = PSOut[i][0];
			float YL = PSOut[i][1];
			float DL2 = std::pow(XL, 2) + std::pow(YL, 2);
			float DL = std::sqrt(DL2);

			if (DL < AMin) {
				continue;
			}

			float Alpha = 1.0f / DL2;
			float Cos = XL / DL;
			float Cos2 = std::pow(Cos, 2);
			float Sin = YL / DL;
			float Sin2 = std::pow(Sin, 2);
			float CK1 = 2.0f * Cos * Sin;
			float A = Cos2 * Alpha + Sin2 * Beta;
			float B = Cos2 * Beta + Sin2 * Alpha;
			float CK = CK1 * (Alpha - Beta);

			for (int j = 0; j < M; j++) {
				float XJ = PSOut[j][0];
				float YJ = PSOut[j][1];

				if (A * std::pow(XJ, 2) + B * std::pow(YJ, 2) + CK * XJ * YJ > 1.00001f) {
					goto main_forloop;
				}
			}

			float Area = M_PI * DL * RaMin;

			if (Area < Area0) {
				Area0 = Area;
				A0 = A;
				B0 = B;
				CK0 = CK;
			}
		}
	
	// Four parameters are returned
	MinAxis1_struct output = { A0, B0, CK0, Area0 };
	return output;
}