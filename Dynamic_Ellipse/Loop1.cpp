#include "pch.h"
#include "Loop1.h"
#include "AngleDiffStationKeepingLibrary.h"
#include "TransformationMatrixLibrary.h"
#include "Constants.h"
#include "Earth2SatLibrary.h"
#include "NormVectorLibrary.h"
#include "AddPointingRotationalErrorsLibrary.h"
#include "EliminateLibrary.h"
#include "PointsCheckEllipseLibrary.h"
#include "3PointsEllipseLibrary.h"
#include "OrientationLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This subroutine loops through the calculation
Loop1_struct Loop1(float& NTot, float& THct, float& PHct, float& dango, float& station_keeping_error, float& pointing_error, float& rotational_error, float& orbital_position, float** PTRect) {

	float Areal = 1000000.0f;

	float Dangl = dango;
	float iflagc = 0;

	float Theta = THct;
	float Phi = PHct;

	float* PBS = new float[2];

	PBS[0] = Theta;
	PBS[1] = Phi;
	float THc = Theta;
	float PHc = Phi;

	if (station_keeping_error != 0) {
		// Call StaKep
		float* AngDif = StaKep(THc, PHc, NTot, PTRect);
	}

	// Call TraMat
	float** MGTOS = TransformationMatrix(orbital_position, THc, PHc).MatrixEC2Sat;
	float** MSTOG = TransformationMatrix(orbital_position, THc, PHc).MatrixSat2EC;

	float* PTEarth = new float[2];

	for (int k = 0; k < NTot; k++) {
		PTEarth[0] = PTRect[k][0] - GEOAlt_EarthRad_rat;
		PTEarth[1] = PTRect[k][1];
		PTEarth[2] = PTRect[k][2];

		// Call Er2Sat
		float* PTSat = Earth2Sat(MGTOS, PTEarth);

		// Call Norm
		float* PTNorm = NormVector(PTSat);
		PTRect[k][0] = PTNorm[0];
		PTRect[k][1] = PTNorm[1];
		PTRect[k][2] = PTNorm[2];
	}

	if ((station_keeping_error != 0) || (pointing_error != 0) || (rotational_error != 0)) {

		// Call XTNDEL
		float** PxRect = XTNDEL(NTot, PTRect, AngDif, XMin, pointing_error, TanPe, CROT, SROT, iSKErr, iPTErr, iRotErr);
		float** PSOut = EliminatePoints(MX, PxRect);
	}

	else {
		// Call Elim
		float** PSOut = EliminatePoints(NTot, PTRect);
	}

	float dang0 = std::pow(Theta - THcg, 2) + std::pow(Phi - PHcg, 2);
	if (iminel == 1) {
		// Call Minchk
		float A0 = PointsCheck(M, AMin, CoefAB, ArMin, A0, B0, CK0, PSOut).A0;
		float B0 = PointsCheck(M, AMin, CoefAB, ArMin, A0, B0, CK0, PSOut).B0;
		float CK0 = PointsCheck(M, AMin, CoefAB, ArMin, A0, B0, CK0, PSOut).CK0;
		float Area0 = PointsCheck(M, AMin, CoefAB, ArMin, A0, B0, CK0, PSOut).Area0;

		if ((Area0 < Areal) || (Area0 == Areal && dang0 < Dangl)) {
			Areal = Area0;
			Al = A0;
			Bl = B0;
			CKl = CK0;
			Thetal = Theta;
			Phil = Phi;
			Dangl = dang0;
			iflagc = 1;
		}
	}

	// Call Threepts
	float A0 = Points3Ellipse(M, PSOut, A0, B0, CK0, CoefAB).A0;
	float B0 = Points3Ellipse(M, PSOut, A0, B0, CK0, CoefAB).B0;
	float CK0 = Points3Ellipse(M, PSOut, A0, B0, CK0, CoefAB).CK0;
	float B0 = Points3Ellipse(M, PSOut, A0, B0, CK0, CoefAB).Area0;

	// Call Orient
	float Alpha = Orient(A0, B0, CK0).Alpha;
	float Beta = Orient(A0, B0, CK0).Beta;
	float Omega = Orient(A0, B0, CK0).Omega;

	if (Alpha <= CoefAB || Beta <= CoefAB) {
		if (Area0 < Areal || (Area0 == Areal && Dang0 < Dangl)) {
			Areal = Area0;
			Al = A0;
			Bl = B0;
			CKl = CK0;
			Thetal = Theta;
			Phil = Phi;
			Dangl = Dang0;
		}
	}

	Loop1_struct output = { Area, A, B, CK, Theta, Phi, Dango };
	return output;
}
