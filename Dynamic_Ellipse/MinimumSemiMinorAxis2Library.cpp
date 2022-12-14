#include "pch.h"
#include "MinimumSemiMinorAxis2Library.h"
#include "TrigonometricEqSolveLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library determines an ellipse with a given minimum semi-major axis
MinAxis2_struct MinAxis2(const long& M, float& A0, float& B0, float& CK0, float& AMin, float** PSOut) {
	// Initialization
	float TH1 = 0.0f;
	float TH2 = 0.0f;
		
	float Area0 = 1000001.0f;
	float Beta2 = 1 / std::pow(AMin,2);
	float RaMin = AMin;
	float RBeta2 = 1 / std::pow(RaMin, 2);

	for (int i = 0; i < M - 1; i++) {
		float X1 = PSOut[i][0];
		float Y1 = PSOut[i][1];
		float R12 = std::pow(X1, 2) + std::pow(Y1, 2);

		if (R12 <= std::pow(AMin, 2)) {
			continue;
		}

		float C1 = 1 / R12;
		float D1 = C1 - Beta2;
		float R1 = std::sqrt(R12);
		float CV = X1 / R1;

		if (CV >= 1) {
			CV = 1.0f;
		}
		if (CV <= -1) {
			CV = -1.0f;
		}

		if (Y1 >= 0) {
			float TH1 = std::acos(CV);
		}
		else {
			float TH1 = std::acos(-CV);
		}

		if (TH1 > M_PI / 2) {
			TH1 = TH1 - M_PI;
		}

		loop_300:
			for (int j = i + 1; j < M; j++) {
				float X2 = PSOut[j][0];
				float Y2 = PSOut[j][1];
				float R22 = std::pow(X2, 2) + std::pow(Y2, 2);

				if (R22 <= std::pow(AMin, 2)) {
					continue;
				}

				float R2 = std::sqrt(R22);

				CV = X2 / R2;

				if (CV >= 1) {
					CV = 1.0f;
				}
				if (CV <= -1) {
					CV = -1.0f;
				}

				if (Y2 >= 0) {
					TH2 = std::acos(CV);
				}
				else {
					TH2 = std::acos(-CV);
				}

				if (TH2 > M_PI / 2) {
					TH2 = TH2 - M_PI;
				}

				if (TH1 - TH2 > M_PI / 2) {
					TH2 = TH2 + M_PI;
				}
				else if (TH2 - TH1 > M_PI / 2) {
					TH1 = TH1 + M_PI;
				}

				// CALL SUBROUTINE
				float Alpha2 = TrigSolve(X1, Y1, X2, Y2, TH1, TH2, AMin).Alpha;
				float THFin = TrigSolve(X1, Y1, X2, Y2, TH1, TH2, AMin).THFIN;

				if (Alpha2 > 0) {
					continue;
				}

				float Sin1 = std::sin(THFin);
				float Sin2 = std::pow(Sin1, 2);
				float Cos1 = std::cos(THFin);
				float Cos2 = std::pow(Cos1, 2);
				float A = Cos2 * Alpha2 + Sin2 * RBeta2;
				float B = Cos2 * Beta2 + Sin2 * Alpha2;
				float CK = 2 * Cos1 * Sin1 * (Alpha2 - RBeta2);

				for (int l = 0; l < M; l++) {
					float XL = PSOut[l][0];
					float YL = PSOut[l][1];

					float Q = A * XL * XL + B * YL * YL + CK * XL * XL;

					if (Q > 1.000001f) {
						goto loop_300;
					}
				}

				if (4 * A * B - CK * CK <= 0) {
					continue;
				}

				float Area = 2.0f * M_PI / std::sqrt(4 * A * B - CK * CK);

				if (Area < Area0) {
					Area0 = Area;
					A0 = A;
					B0 = B;
					CK0 = CK;
				}
			}
	}

	// Four parameters are returned
	MinAxis2_struct output = { A0, B0, CK0, Area0 };
	return output;
}