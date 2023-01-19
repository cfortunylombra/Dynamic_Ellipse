#include "pch.h"
#include "AngleDiffStationKeepingLibrary.h"
#include "Polar2RectangularLibrary.h"
#include "Constants.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library calculates angle differences to take care of the station keeping error
float* StaKep(float& ThC, float& PhC, float& N, float** PtRect, float** COSCOS, float** COSSIN, float** SIN) {
	//Initalization
	float** PC;
	PC = new float*[4];
	for (int i = 0; i < 4; i++) {
		PC[i] = new float[2];
	}
	float Omega = 0.0f;
	float Pap = 0.0f;
	float DotP = 0.0f;
	float OmegaP = 0.0f;
	float DifMax = 0.0f;
	float* AngDif = new float[N];

	float RK2 = GEOAlt_EarthRad_rat * GEOAlt_EarthRad_rat;

	// Call PoltoRec
	float XC = Pol2Rect(ThC, PhC).Xout;
	float YC = Pol2Rect(ThC, PhC).Yout;
	float ZC = Pol2Rect(ThC, PhC).Zout;

	float PC0 = std::sqrt(1 + RK2 - 2 * XC * GEOAlt_EarthRad_rat);

	//For-loop
	for (int ind2 = 0; ind2 < 2; ind2++) {
		//For-loop
		for (int i = 0; i < 4; i++) {
			PC[i][ind2] = std::sqrt(1.0 + RK2 - 2.0 * GEOAlt_EarthRad_rat * (XC * COSCOS[i][ind2] + YC * COSSIN[i][ind2] + ZC * SIN[i][ind2]));
		}
	}

	//For-loop
	for (int i = 0; i < N; i++) {
		float Xa = PtRect[i][0];
		float Ya = PtRect[i][1];
		float Za = PtRect[i][2];

		float Pa = std::sqrt(1.0 + RK2 - 2.0 * Xa * GEOAlt_EarthRad_rat);
		float PRod = Xa * XC + Ya * YC + Za * ZC + RK2;
		float Dot = PRod - GEOAlt_EarthRad_rat * (Xa + XC);

		float Angl = Dot / (2 * Pa * PC0);

		if (Angl >= 1.0) {
			Omega = 0.0f;
		}
		else if (Angl <= -1.0) {
			Omega = Pi;
		}
		else {
			Omega = std::acos(Angl);
		}

		float DifMax = 0.0f;

		for (int ind2 = 0; ind2 < 2; ind2++) {
			for (int j = 0; j < 4; j++) {
				Pap = std::sqrt(1.0 + RK2 - 2.0 * GEOAlt_EarthRad_rat * (Xa * COSCOS[j][ind2] + Ya * COSSIN[j][ind2] + Za * SIN[j][ind2]));
			
				DotP = PRod - GEOAlt_EarthRad_rat * ((Xa + XC) * COSCOS[j][ind2] + (Ya + YC) * COSSIN[j][ind2] + (Za + ZC) * SIN[j][ind2]);
			
				Angl = DotP / (2.0f * Pap * PC[j][ind2]);

				if (Angl > 1.0) {
					OmegaP = 0.0f;
				}
				else if (Angl <= -1.0) {
					OmegaP = Pi;
				}
				else {
					OmegaP = std::acos(Angl);
				}

				if (OmegaP > Omega) {
					float Dif = OmegaP - Omega;
					if (Dif > DifMax) {
						DifMax = Dif;
					}
				}
			}
		}

		AngDif[i] = DifMax;
	}

	return AngDif;
}