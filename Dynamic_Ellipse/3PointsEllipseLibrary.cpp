#include "pch.h"
#include "3PointsEllipseLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library determines an ellipse containing 3 points of psout and encompassing the rest m-3
Points3Ellipse_struct Points3Ellipse(const long& M, float** PSOut, float& A0, float& B0, float& CK0, float& A, float& B, float& CK, float& CoefAB) {
	float Area0 = 1000001.0f;

	for (int i = 0; i < M - 2; i++) {
		float X1 = PSOut[i][0];
		float Y1 = PSOut[i][1];

		for (int j = i + 1; j < M - 1; j++) {
			float X2 = PSOut[j][0];
			float Y2 = PSOut[j][1];

			loop_200:
				for (int k = j + 1; k < M; k++) {
					float X3 = PSOut[k][0];
					float Y3 = PSOut[k][1];

					// Call Subroutine


					if ((A <= 0) || (B <= 0) || (4 * A * B - CK * CK <= 0)) {
						continue;
					}

					if (A + B - std::sqrt(std::pow(A - B, 2) + CK * CK) > 2 * CoefAB) {
						continue;
					}

					for (int l = 0; l < M; l++) {
						float XL = PSOut[l][0];
						float YL = PSOut[l][1];
						float Q = A * XL * XL + B * YL * YL + CK * XL * YL;
				
						if (Q > 1.00001f) {
							goto loop_200;
						}
					}

					float Area = 2 * M_PI / std::sqrt(4 * A * B - CK * CK);
				
					if (Area < Area0) {
						Area0 = Area;
						A0 = A;
						B0 = B;
						CK0 = CK;
					}
				}
		}
	}

	// Four parameters are returned
	Points3Ellipse_struct output = { Area0, A0, B0, CK0 };
	return output;
}