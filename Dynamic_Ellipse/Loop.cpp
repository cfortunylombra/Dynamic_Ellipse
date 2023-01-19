#include "pch.h"
#include "Loop.h"
#include "Loop1.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This subroutine loops through the calculation
Loop_struct Loop(float& NTot, float** PTRect, float& PHMaxx, float& PHMinn, float& THMaxx, float& THMinn, float& thcg, float& station_keeping_error, float& pointing_error, float& rotational_error, float& orbital_position, float** COSCOS, float** COSSIN, float** SIN, float& M, float& XMin, float& AMin, float& ArMin, float& CoefAB) {
	float Areal = 1000000.0f;
	float Dangl = 1000000.0f;

	float Al = 0.0f;
	float Bl = 0.0f;
	float CKl = 0.0f;
	float Thetal = 0.0f;
	float Phil = 0.0f;
	float Arel = Areal;

	float DistPh = (PHMaxx - PHMinn) / 2.0f;
	float DistTh = (THMaxx - THMinn) / 2.0f;
	float Distp = DistPh;
	float Distt = DistTh;
	float Thmi = THMinn + Distt;
	Distt = 0.4f * Distt;
	float Dang = Dangl;

	Distp = DistPh;
	float Ph1 = PHMinn + Distp;
	float Ph2 = Ph1 - 0.4f * Distp;
	float Ph3 = Ph1 + 0.4f * Distp;

	float Armin = 1000000.0f;
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
		float Area1 = Loop1(NTot, Th1, Ph1, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Area;
		float A1 = Loop1(NTot, Th1, Ph1, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).A;
		float B1 = Loop1(NTot, Th1, Ph1, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).B;
		float CK1 = Loop1(NTot, Th1, Ph1, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).CK;
		float Theta = Loop1(NTot, Th1, Ph1, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Theta;
		float Phi = Loop1(NTot, Th1, Ph1, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Phi;

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
		float Area2 = Loop1(NTot, Th1, Ph2, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Area;
		float A2 = Loop1(NTot, Th1, Ph2, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).A;
		float B2 = Loop1(NTot, Th1, Ph2, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).B;
		float CK2 = Loop1(NTot, Th1, Ph2, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).CK;
		Theta = Loop1(NTot, Th1, Ph2, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Theta;
		Phi = Loop1(NTot, Th1, Ph2, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Phi;

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
		float Area3 = Loop1(NTot, Th1, Ph3, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Area;
		float A3 = Loop1(NTot, Th1, Ph3, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).A;
		float B3 = Loop1(NTot, Th1, Ph3, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).B;
		float CK3 = Loop1(NTot, Th1, Ph3, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).CK;
		Theta = Loop1(NTot, Th1, Ph3, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Theta;
		Phi = Loop1(NTot, Th1, Ph3, Dang, station_keeping_error, pointing_error, rotational_error, orbital_position, PTRect, COSCOS, COSSIN, SIN, Al, Bl, CKl, M, XMin, AMin, ArMin, CoefAB).Phi;

		if ((Area3 < Areal) || ((Area3 == Areal) && Dang < Dangl)) {
			Areal = Area3;
			Al = A3;
			Bl = B3;
			CKl = CK3;
			Thetal = Theta;
			Phil = Phi;
			Dangl = Dang;
		}

		Distp = 0.6f * Distp;
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

		if (Distp < 0.000006f) {
			if ((Arel < Armin) || ((Arel == Armin) && std::abs(Thmi - thcg) > std::abs(Th1 - thcg))) {
				Armin = Arel;
				Thmi = Th1;
			}
		}
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