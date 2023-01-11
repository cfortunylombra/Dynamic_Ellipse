#include "pch.h"
#include "AddPointingRotationalErrorsLibrary.h"
#include "Constants.h"
#include "AddStationKeepingErrorLibrary.h"
#include "AddPointingErrorLibrary.h"
#include "AddRotationalErrorLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library extends the points to be considered so that the various pointing and rotational errors can be taken into account
float** XTNDEL(float& N, float** PSRect, float* AngDif, float& XMin, float& XPtErr, float& TanPe, float& CROT, float& SROT, float& iSKErr, float& iPTErr, float& iRotErr) {
	// Initialization
	float Mx = N;
	float* PTIn = new float[3];
	float** PTMed1;
	PTMed1 = new float* [400];
	for (int i = 0; i < 400; i++) {
		PTMed1[i] = new float[3];
	}

	float** PTMed2;
	PTMed2 = new float* [400];
	for (int i = 0; i < 400; i++) {
		PTMed2[i] = new float[3];
	}

	float** PxRect;
	PxRect = new float* [400];
	for (int i = 0; i < 400; i++) {
		PxRect[i] = new float[3];
	}

	// Check Station Keeping Error is 1 (True)
	if (iSKErr == 1) {
		// For-loop
		for (int i = 0; i < 100; i++) {
			// For-loop
			for (int j=0; j<3; j++){
				PTIn[j] = PSRect[i][j];
			}

			float DifAng = AngDif[i];
			// Call subroutine SKErr
			float* PTOut = SkError(PTIn, DifAng);

			// For-loop
			for (int j = 0; j < 3; j++) {
				PTMed1[i][j] = PTOut[j];
			}
		}
	}
	// No Station Keeping Error
	else {
		// For-loop
		for (int i = 0; i < Mx; i++) {
			// For-loop
			for (int j = 0; j < 3; j++) {
				PTMed1[i][j] = PSRect[i][j];
			}
		}
	}

	// Check Pointing Error is 1 (True)
	int NTot = -1;
	if (iPTErr == 1) {
		// For-loop
		for (int i = 0; i < Mx; i++) {
			// For-loop
			for (int j = 0; j < 3; j++) {
				PTIn[j] = PTMed1[i][j];
			}

			float XDif = XMin - XPtErr;
			if (XDif <= 0) {
				XDif = 0;
				if (PTIn[0] * PTIn[0] + PTIn[1] * PTIn[1] < XDif * XDif / Rad / Rad) {
					NTot = NTot + 1;
					// For-loop
					for (int j = 0; j < 3; j++) {
						PTMed2[NTot][j] = PTIn[j];
					}
				}
				else {
					// Call subroutine PTErr
					float** PT7Out = PtError(PTIn, TanPe);

					// For-loop
					for (int k = 0; k < 7; k++) {
						NTot = NTot + 1;
						// For-loop
						for (int j = 0; j < 3; j++) {
							PTMed2[NTot][j] = PT7Out[k][j];
						}
					}
				}
			}

			else {
				Mx = NTot;
				// For-loop
				for (int i = 0; i < Mx; i++) {
					NTot = NTot + 1;

					// For-loop
					for (int j = 0; j < 3; j++) {
						PTMed2[i][j] = PTMed1[i][j];
					}
				}
			}
			
			Mx = NTot;

			if (iRotErr == 1) {
				NTot = -1;

				// For-loop
				for (int i = 0; i < Mx; i++) {
					// For-loop
					for (int j = 0; j < 3; j++) {
						PTIn[j] = PTMed2[i][j];
					}

					// Call subroutine RotErr
					float** PT3Out = RotError(PTIn, CROT, SROT);

					// For-loop
					for (int k = 0; k < 3; k++) {
						NTot = NTot + 1;

						// For-loop
						for (int j = 0; j < 3; j++) {
							PxRect[NTot][j] = PT3Out[k][j];
						}
					}
				}

				Mx = NTot;
			}

			else {
				NTot = 0;

				// For-loop
				for (int i = 0; i < Mx; i++) {
					NTot = NTot + 1;

					// For-loop
					for (int j = 0; j < 3; j++) {
						PxRect[i][j] = PTMed2[i][j];
					}
				}

				Mx = NTot;
			}
		}
	}

	return PxRect;
}