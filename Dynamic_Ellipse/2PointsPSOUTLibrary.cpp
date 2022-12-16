#include "pch.h"
#include "2PointsPSOUTLibrary.h"
#include "DeterminantLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library determines an ellipse having the smallest area, containing two points of psout, and encompassing the rest (m-2)
TwoPts_struct TwoPts(const long& M, float** PSOut, float& A0, float& B0, float& CK0, float& CoefAB) {
	float Area0 = 1000001.0f;

	for (int i = 0; i < M - 1; i++) {
		float X1 = PSOut[i][0];
		float Y1 = PSOut[i][1];

		loop_300:
			for (int j = i + 1; j < M; j++) {
				float X2 = PSOut[j][0];
				float Y2 = PSOut[j][1];

				// Call subroutine
				float A = Det(X1, Y1, X2, Y2).A;
				float B = Det(X1, Y1, X2, Y2).B;
				float CK = Det(X1, Y1, X2, Y2).CK;

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

					if (Q > 1.00001) {
						goto loop_300;
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

	// Return 4 parameters of the ellipse
	TwoPts_struct output = { A0, B0, CK0, Area0 };
	return output;
}