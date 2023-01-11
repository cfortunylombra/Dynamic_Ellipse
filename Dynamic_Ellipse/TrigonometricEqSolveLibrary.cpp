#include "pch.h"
#include "TrigonometricEqSolveLibrary.h"
#include <iostream>
#include <algorithm>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library solves trigonometric equations approximately using double precision
TrigSolve_struct TrigSolve(float& X1, float& Y1, float& X2, float& Y2, float& TH1, float& TH2, float& AMin) {
	//Initialization
	float THFIN = 0.0f;
	float THFIN1 = 0.0f;
	float THFIN2 = 0.0f;
	float Alpha = 0.0f;
	float Alpha1 = 0.0f;
	float Alpha2 = 0.0f;

	float RaMin = AMin;
	float RBeta = 1 / (RaMin * RaMin);

	float RTH1 = TH1;
	float RTH2 = TH2;

	float RX1 = X1;
	float RY1 = Y1;
	float RX2 = X2;
	float RY2 = Y2;

	float RC1 = 1 / (std::pow(RX1, 2) + std::pow(RY1, 2));
	float RD1 = RC1 - RBeta;
	float RC2 = 1 / (std::pow(RX2, 2) + std::pow(RY2, 2));
	float RD2 = RC2 - RBeta;

	float R1 = std::sqrt(-RD1);
	float R2 = std::sqrt(-RD2);
	float RNU1 = R1 * std::cos(RTH2) + R2 * std::cos(RTH1);
	float RDE1 = R1 * std::sin(RTH2) + R2 * std::sin(RTH1);
	float RNU2 = R1 * std::cos(RTH2) - R2 * std::cos(RTH1);
	float RDE2 = R1 * std::sin(RTH2) - R2 * std::sin(RTH1);

	if (RDE1 == 0.0f) {
		if (RNU1 >= 0.0f) {
			THFIN1 = -M_PI / 2;
		}
		else if (RNU1 < 0.0f) {
			THFIN1 = M_PI / 2;
		}
	}
	else {
		THFIN1 = std::atan2(-RNU1, RDE1);
	}

	if ((std::abs(THFIN1 - RTH1) < M_PI / 2) && (std::abs(THFIN1 - RTH1) > M_PI / 2)) { //REDUNDANT THIS IF STATEMENT
		float Alpha1 = RC1 + RD1 * std::pow(std::tan(-M_PI / 2), 2);
	}
	else {
		float Alpha1 = RC1 + RD1 * std::pow(std::tan(THFIN1 - RTH1), 2);
	}

	if (RDE2 == 0.0f) {
		if (RNU2 >= 0.0f) {
			float THFIN2 = -M_PI / 2;
		}
		else {
			float THFIN2 = M_PI / 2;
		}
	}
	else {
		float THFIN2 = std::atan2(-RNU2, RDE2);
	}

	if ((std::abs(THFIN2-RTH1)<M_PI/2) && (std::abs(THFIN2-RTH1)>M_PI/2)) { //REDUNDANT THIS IF STATEMENT
		float Alpha2 = RC1 + RD1 * std::pow(std::tan(-M_PI / 2), 2);
	}
	else {
		float Alpha2 = RC1 + RD1 * std::pow(std::tan(THFIN2-RTH1), 2);
	}

	if (Alpha1 > 0) {
		float Alpha = Alpha1;
		float THFIN = THFIN1;
	}

	else if (Alpha2 > 0) {
		float Alpha = Alpha2;
		float THFIN = THFIN2;
	}

	else {
		float Alpha = -1.0f;
	}

	// Two parameters are returned
	TrigSolve_struct output = { Alpha, THFIN };
	return output;
}