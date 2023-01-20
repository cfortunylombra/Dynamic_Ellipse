#include "pch.h"
#include "ProjectionSatEllipseLibrary.h"
#include "TransformationMatrixLibrary.h"
#include "EdgeEllipseLibrary.h"
#include "XPrimeYPrime2XYLibrary.h"
#include "Sat2EarthLibrary.h"
#include "Constants.h"
#include <iostream>

// Definition: This library projects satellite ellipse onto earth
float** Proj(float& Theta, float& Phi, float& Alpha, float& Beta, float& Omega, float& SatLon, float* PtSat) {
	float** Ang;
	Ang = new float* [26];
	for (int i = 0; i < 26; i++) {
		Ang[i] = new float[2];
	}

	float FAC = 0.0f;

	float RK2 = std::pow(GEOAlt_EarthRad_rat,2);

	// Call TraMat
	float** MsTog = TransformationMatrix(SatLon, Theta, Phi).MatrixSat2EC;

	// Call ElPoly
	float** ElPts = ElPoly(Alpha, Beta);

	PtSat[2] = 1.0f;
	
	//for-loop
	for (int i = 0; i < 36; i++) {
		float XPrI = ElPts[i][0];
		float YPrI = ElPts[i][1];

		// Call rotate
		float X = Rotate(XPrI, YPrI, Omega).X;
		float Y = Rotate(XPrI, YPrI, Omega).Y;

		PtSat[0] = X;
		PtSat[1] = Y;

		// Call SatoErt
		float* PtErth = Sat2Earth(MsTog, PtSat);

		float Xe = PtErth[0];
		float Ye = PtErth[1];
		float Ze = PtErth[2];

		float R2 = Xe * Xe + Ye * Ye + Ze * Ze;
		float Qt = Xe * Xe * RK2 - (RK2 - 1) * R2;

		if (Qt >= 0) {
			FAC = (-Xe * GEOAlt_EarthRad_rat - std::sqrt(Qt)) / R2;
			Xe = GEOAlt_EarthRad_rat + FAC * Xe;
			Ye = FAC * Ye;
			Ze = FAC * Ze;
		}

		else {
			float PlRot = MsTog[0][0] * X + MsTog[0][2] * Y;
			float ACoef = (X * X + Y * Y) * (RK2 - 1.0f) - RK2 * PlRot * PlRot;
			float BCoef = -2.0f * RK2 * MsTog[0][2] * PlRot;
			float CCoef = (RK2 - 1.0f) - RK2 * MsTog[0][2] * MsTog[0][2];

			float SQT = std::sqrt(BCoef * BCoef - 4.0f * ACoef * CCoef);
			float RLamb = (-BCoef + SQT) / (2.0f * ACoef);

			if (RLamb < 0) {
				RLamb = (-BCoef - SQT) / (2.0f * ACoef);
			}

			PtSat[0] = X * RLamb;
			PtSat[1] = Y * RLamb;

			// Call SatoErt

			Xe = PtErth[0];
			Ye = PtErth[1];
			Ze = PtErth[2];

			R2 = Xe * Xe + Ye * Ye + Ze * Ze;
			float QT = Xe * Xe * RK2 - (RK2 - 1.0f) * R2;

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