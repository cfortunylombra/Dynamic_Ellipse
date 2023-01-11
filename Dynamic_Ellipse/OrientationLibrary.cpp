#include "pch.h"
#include "OrientationLibrary.h"
#include "Constants.h"
#include <iostream>

// Definition: This library determines the ellipse orientation
Orient_struct Orient(float& A, float& B, float& CK) {
	//Initalizating Omega parameter
	float Omega = 0.0;

	float SurD = std::sqrt(CK * CK + (A - B) * (A - B));
	float Alpha = (A + B - SurD) / 2;
	float Beta = (A + B + SurD) / 2;

	if (A == B) {
		if (CK == 0) {
			Omega = 0.0;
		}
		else if (CK < 0) {
			Omega = 45.0;
		}
		else {
			Omega = 135.0;
		}
	}
	else if (A > B) {
		Omega = 0.5 * Rad * std::atan(CK / (A - B)) + 90.0;
	}
	else if (A < B) {
		Omega = 0.5 * Rad * std::atan(CK / (A - B));
	}
	if (Omega < 0) {
		Omega = Omega + 180.0;
	}


	// Return three parameters (Alpha, Beta and Omega)
	Orient_struct output = { Alpha, Beta, Omega };
	return output;
}