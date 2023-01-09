#include "pch.h"
#include "AddPointingRotationalErrorsLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library extends the points to be considered so that the various pointing and rotational errors can be taken into account
float** Sol3LinEq(float& N, float** PSRect, float* AngDif, float& iSKErr) {
	// Initialization
	float Mx = N;
	float* PTIn = new float[3];
	float** PTMed1;
	PTMed1 = new float* [3];
	for (int i = 0; i < 3; i++) {
		PTMed1[i] = new float[3];
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
			//float* PTOut = SKErr(PTIn, DifAng);

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
	float NTot = 0;
	if (iPTErr == 1) {
		// For-loop
		for (int i = 0; i < Mx; i++) {
			// For-loop
			for (int j = 0; j < 3; j++) {
				PTIn[j] = PTMed1[i][j];
			}

			float XDif = XMin - XPtErr;
			if (Xdif <= 0) {
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
					// float** PT7Out = PTErr(PTIn);

					// For-loop
					for (int k = 0; k < 7; k++) {
						NTot = NTot + 1;
					}
					// For-loop
					for (int j = 0; j < 3; j++) {
						PTMed2[NTot][j] = PT7Out[k][j];
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
				NTot = 0;

				// For-loop
				for (int i = 0; i < Mx; i++) {
					// For-loop
					for (int j; j < 3; j++) {
						PTIn[j] = PTMed2[i][j];
					}

					// Call subroutine RotErr
					//

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

	return;
}