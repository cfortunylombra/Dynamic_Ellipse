#include "pch.h"
#include "EllipseViewLibrary.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library gives a primitive view of ellipse
float** Draw(float& N, float& Theta, float& Phi, float& SatLon, float** PtsGeo, float** Ang) {
	//Initialization
	int ITh = 0.0f;
	int IPh = 0.0f;
	
	float DelPhi = Phi - SatLon;
	
	if (DelPhi <= -180) {
		Phi = Phi + 360.0f;
	}

	else if (DelPhi >= 180) {
		Phi = Phi - 360.0f;
	}

	char ChBL = ' ';
	char ChX = 'X';
	char ChY = 'Y';
	char ChZ = 'Z';
	char ChO = 'O';

	float ThMax = -1000.0f;
	float ThMin = 1000.0f;
	float PhMax = -1000.0f;
	float PhMin = 1000.0f;

	//For-loop
	for (int i = 0; i < 36; i++) {
		if (ThMax < Ang[i][0]) {
			ThMax = Ang[i][0];
		}
		if (ThMin > Ang[i][0]) {
			ThMin = Ang[i][0];
		}
		if (PhMax < Ang[i][1]) {
			PhMax = Ang[i][1];
		}
		if (PhMin > Ang[i][1]) {
			PhMin = Ang[i][1];
		}
	}

	//For-loop
	for (int i = 0; i < N; i++) {
		if (ThMax < PtsGeo[i][0]) {
			ThMax = PtsGeo[i][0];
		}
		if (ThMin > PtsGeo[i][0]) {
			ThMin = PtsGeo[i][0];
		}
		if (PhMax < PtsGeo[i][1]) {
			PhMax = PtsGeo[i][1];
		}
		if (PhMin > PtsGeo[i][1]) {
			PhMin = PtsGeo[i][1];
		}
	}

	float DTh = ThMax - ThMin;
	float DPh = PhMax - PhMin;

	float** Ch;
	Ch = new float* [51];
	for (int i = 0; i < 51; i++) {
		Ch[i] = new float[76];
	}

	for (int i = 0; i < 76; i++) {
		for (int j = 0; j < 51; j++) {
			Ch[j][i] = ChBL;
		}
	}

	for (int i = 0; i < 36; i++) {
		ITh = 50.0f * (ThMax - Ang[i][0]) / DTh + 1.0001;
		IPh = 75.0f * (PtsGeo[i][1] - PhMin) / DPh + 1.0001;

		if (Ch[ITh][IPh] == ChX) {
			Ch[ITh][IPh] = ChZ;
		}
		else {
			Ch[ITh][IPh] = ChY;
		}
	}

	return Ch;
}