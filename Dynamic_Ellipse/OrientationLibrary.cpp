#include "pch.h"
#include "OrientationLibrary.h"
#include "Constants.h"
#include <iostream>

// Definition: This library determines the ellipse orientation
Orient_struct Orient(float& A, float& B, float& CK) {
	//Initalizating Omega parameter
	float Omega = 0.0f;

	float SurD = std::sqrt(CK * CK + (A - B) * (A - B));
	float Alpha = (A + B - SurD) / 2.0f;
	float Beta = (A + B + SurD) / 2.0f;

	if (A == B) {
		if (CK == 0) {
			Omega = 0.0f;
		}
		else if (CK < 0) {
			Omega = 45.0f;
		}
		else {
			Omega = 135.0f;
		}
	}
	else if (A > B) {
		Omega = 0.5f * Rad * std::atan(CK / (A - B)) + 90.0f;
	}
	else if (A < B) {
		Omega = 0.5f * Rad * std::atan(CK / (A - B));
	}
	if (Omega < 0) {
		Omega = Omega + 180.0f;
	}


	// Return three parameters (Alpha, Beta and Omega)
	Orient_struct output = { Alpha, Beta, Omega };
	return output;
}