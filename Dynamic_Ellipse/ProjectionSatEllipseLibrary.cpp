#include "pch.h"
#include "ProjectionSatEllipseLibrary.h"
#include "Constants.h"
#include <iostream>

// Definition: This library projects satellite ellipse onto earth
float** Project(float& Theta, float& Phi, float& Alpha, float& Beta, float& Omega) {
	float** Ang;
	Ang = new float* [26];
	for (int i = 0; i < 26; i++) {
		Ang[i] = new float[2];
	}

	float RK2 = std::pow(GEOAlt_EarthRad_rat,2);

	// Call TraMat

	// Call ElPoly

	PtSat[2] = 1.0f;
	
	//for-loop
	for (int i = 0, i < 36; i++) {
		XPrI = ElPts[i][0];
		YPrI = ElPts[i][1];

		// Call rotate
		PtSat[0] = X;
		PtSat[1] = Y;

		// Call SatoErt

		float Xe = PtErth[0];
		float Ye = PtErth[1];
		float Ze = PtErth[2];

		float R2 = Xe * Xe + Ye * Ye + Ze * Ze;
		float Qt = Xe * Xe * RK2 - (RK2 - 1) * R2;

		if (Qt >= 0) {
			float FAC = (-Xe * GEOAlt_EarthRad_rat - std::sqrt(Qt)) / R2;
			Xe = GEOAlt_EarthRad_rat + FAC * Xe;
			Ye = FAC * Ye;
			Ze = FAC * Ze;
		}

		else {
			float PlRot = MsTog[0][0] * X + MsTog[0][2] * Y;
			float ACoef = (X * X + Y * Y) * (RK2 - 1) - RK2 * PlRot * PlRot;
			float BCoef = -2 * RK2 * MsTog[0][2] * PlRot;
			float CCoef = (RK2 - 1) - RK2 * MsTog[0][2] * MsTog[0][2];

			float SQT = std::sqrt(BCoef * BCoef - 4 * ACoef * CCoef);
			float RLamb = (-BCoef + SQT) / (2 * ACoef);

			if (RLamb < 0) {
				RLamb = (-BCoef - SQT) / (2 * ACoef);
			}

			PtSat[0] = X * RLamb;
			PtSat[1] = Y * RLamb;

			// Call SatoErt

			Xe = PtErth[0];
			Ye = PtErth[1];
			Ze = PtErth[2];

			R2 = Xe * Xe + Ye * Ye + Ze * Ze;
			float QT = Xe * Xe * RK2 - (RK2 - 1) * R2;

			FAC = -Xe * GEOAlt_EarthRad_rat / R2;
			Xe = GEOAlt_EarthRad_rat + FAC * Xe;
			Ye = FAC * Ye;
			Ze = FAC * Ze;
		}

		float TH = std::asin(Ze);
		Ang[i][0] = TH * Rad;
		Ang[i][1] = std::asin(Ye / std::cos(TH)) * Rad + SatLon;
	}

	return Ang;
}