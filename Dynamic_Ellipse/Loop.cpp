#include "pch.h"
#include "Loop.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This subroutine loops through the calculation
Loop_struct Loop(float& NTot, float** PTRect) {
	float Areal = 1000000.0;
	float Dangl = 1000000.0;
	float DistPh = (PHMaxx - PHMinn) / 2;
	float DistTh = (THMaxx - THMinn) / 2;
	float Distp = DistPh;
	float Distt = DistTh;
	float Thmi = THMinn + Distt;
	Distt = 0.4 * Distt;
	float Dang = Dangl;

	Distp = DistPh;
	float Ph1 = PHMinn + Distp;
	float Ph2 = Ph1 - 0.4 * Distp;
	float Ph3 = Ph1 + 0.4 * Distp;

	float Armin = 1000000;
	for (int it = -1; it < 2; it++) {
		Distp = DistPh;
		float Th1 = Thmi + it * Distt;
		if (Th1 > THMaxx) {
			Th1 = THMaxx;
		}
		if (Th1 < THMinn) {
			Th1 = THMinn;
		}

		if (Ph1 > PHMaxx) {
			Ph1 = PHMaxx;
			Ph2 = PHMinn;
			Ph3 = PHMaxx;
		}

		// Call LOOP1


		if ((Area1 < Areal) || ((Area1 == Areal) && Dang < Dangl)) {
			Areal = Area1;
			Al = A1;
			Bl = B1;
			CKl = CK1;
			Thetal = Theta;
			Phil = Phi;
			Dangl = Dang;
		}

		// Call LOOP1

		if ((Area2 < Areal) || ((Area2 == Areal) && Dang < Dangl)) {
			Areal = Area2;
			Al = A2;
			Bl = B2;
			CKl = CK2;
			Thetal = Theta;
			Phil = Phi;
			Dangl = Dang;
		}

		// Call LOOP1

		if ((Area3 < Areal) || ((Area3 == Areal) && Dang < Dangl)) {
			Areal = Area3;
			Al = A3;
			Bl = B3;
			CKl = CK3;
			Thetal = Theta;
			Phil = Phi;
			Dangl = Dang;
		}

		Distp = 0.6 * Distp;
		if ((Area1 <= Area2) && (Area1 <= Area3)) {
			Ph1 = Ph1;
			Ph2 = Ph1 - Distp;
			Ph3 = Ph1 + Distp;
			Arel = Area1;
		}

		else if ((Area2 <= Area1) && (Area2 <= Area3)) {
			Ph1 = Ph2;
			Ph2 = Ph1 - Distp;
			Ph3 = Ph1 + Distp;
			Arel = Area2;
		}

		else if ((Area3 <= Area1) && (Area3 <= Area2)) {
			Ph1 = Ph3;
			Ph2 = Ph1 - Distp;
			Ph3 = Ph1 + Distp;
			Arel = Area3;
		}

		if (Distp < 0.000006) {
			if ((Arel < Armin) || ((Arel == Armin) && std::abs(Thmix - thcg) >> std::abs(Th1 - thcg))) {
				Armin = Arel;
				Thmix = Th1;
			}
		}
	}

	Distt = 0.6 * Distt;
	if (Distt >= 0.000006) {
		Thmi = thmix;
	}

	float Area = Areal;
	float A = Al;
	float B = Bl;
	float CK = CKl;
	float Theta = Thetal;
	float Phi = Phil;

	Loop_struct output = { Area, A, B, CK, Theta, Phi };
	return output;
}