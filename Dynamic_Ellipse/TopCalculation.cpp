#include "pch.h"
#include "TopCalculation.h"
#include "Constants.h"
#include "CenterGravityLibrary.h"
#include "UpdateLongitudeLibrary.h"
#include "ConvexLibrary.h"
#include "Polar2RectangularLibrary.h"
#include "OrientationLibrary.h"
#include "ProjectionSatEllipseLibrary.h"
#include "EllipseViewLibrary.h"
#include "Loop.h"
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: Merge of all the libraries
Calc_struct Calc(long& status, float& pointing_error, float& rotational_error, float& station_keeping_error, float& minimum_axis, float& orbital_position, float& n_points, float points_lat[], float points_long[], float** COSCOS, float** COSSIN, float** SIN, float& M, float& AMin, float& ArMin, float& CoefAB) {
	
	// Call CenterGravity
	float theta_cg = CenterGravity(n_points, orbital_position, points_lat, points_long).theta_cg;
	float phi_cg = CenterGravity(n_points, orbital_position, points_lat, points_long).phi_cg;

	// Call UpdateLongitude
	points_long = UpdateLongitude(n_points, orbital_position, points_long);

	float NT = n_points;

	float THMaxx = -1000000.0f;
	float THMinn = 1000000.0f;
	float PHMaxx = -1000000.0f;
	float PHMinn = 1000000.0f;

	for (int i = 0; i < n_points; i++) {

		THMaxx = (std::max)(THMaxx, points_lat[i]);
		THMinn = (std::min)(THMinn, points_lat[i]);

		PHMaxx = (std::max)(PHMaxx, points_long[i]);
		PHMinn = (std::min)(PHMinn, points_long[i]);
	}

	float DK2 = std::pow(GEOAlt_EarthRad_rat,2) - 1.0;

	float** Coords;
	Coords = new float* [NT];
	for (int i = 0; i < NT; i++) {
		Coords[i] = new float[2];
		Coords[i][0] = points_lat[i];
		Coords[i][1] = points_long[i];
	}

	// Call Convex
	float NTin = NT;
	float** Coords_new = Convex(Coords, NTin).Coords_new;
	int NTout = Convex(Coords, NTin).NOut;
	NT = NTout;

	float** PTRect;
	PTRect = new float* [NT];
	float** PTSph;
	PTSph = new float* [NT];
	for (int i = 0; i < NT; i++) {
		PTRect[i] = new float[3];
		PTSph[i] = new float[2];
	}

	for (int j = 0; j < NT; j++) {
		float THin = Coords_new[j][0];
		float PHin = Coords_new[j][1] - orbital_position;

		// Call Pol2Rect
		float XOut = Pol2Rect(THin, PHin).Xout;
		float YOut = Pol2Rect(THin, PHin).Yout;
		float ZOut = Pol2Rect(THin, PHin).Zout;

		float Dist2 = std::pow(GEOAlt_EarthRad_rat - XOut, 2) + std::pow(YOut, 2) + std::pow(ZOut, 2);
		
		if (Dist2 > DK2) {
			std::cout << "Some Points not visible from satellite." << std::endl;
			std::cout << "\nERROR 404" << std::endl;
		}

		if (station_keeping_error != 0) {
			for (int ki = 0; ki < 4; ki++) {
				if (Dist2 < std::pow(XOut - GEOAlt_EarthRad_rat * COSCOS[ki][0], 2) + std::pow(YOut - GEOAlt_EarthRad_rat * COSSIN[ki][0], 2) + std::pow(ZOut - GEOAlt_EarthRad_rat * SIN[ki][0], 2)) {
					Dist2 = std::pow(XOut - GEOAlt_EarthRad_rat * COSCOS[ki][0], 2) + std::pow(YOut - GEOAlt_EarthRad_rat * COSSIN[ki][0], 2) + std::pow(ZOut - GEOAlt_EarthRad_rat * SIN[ki][0], 2);
				}
			}

			for (int ki = 0; ki < 4; ki++) {
				if (Dist2 < std::pow(XOut - GEOAlt_EarthRad_rat * COSCOS[ki][1], 2) + std::pow(YOut - GEOAlt_EarthRad_rat * COSSIN[ki][1], 2) + std::pow(ZOut - GEOAlt_EarthRad_rat * SIN[ki][1], 2)) {
					Dist2 = std::pow(XOut - GEOAlt_EarthRad_rat * COSCOS[ki][1], 2) + std::pow(YOut - GEOAlt_EarthRad_rat * COSSIN[ki][1], 2) + std::pow(ZOut - GEOAlt_EarthRad_rat * SIN[ki][1], 2);
				}
			}
		}

		if (Dist2 > DK2) {
			std::cout << "\nERROR 404" << std::endl;
		}

		PTRect[j][0] = XOut;
		PTRect[j][1] = YOut;
		PTRect[j][2] = ZOut;
		PTSph[j][0] = THin;
		PTSph[j][1] = PHin;
	}

	// Call Loop
	float Area = Loop(n_points, PTRect, PHMaxx, PHMinn, THMaxx, THMinn, theta_cg, station_keeping_error, pointing_error, rotational_error, orbital_position, COSCOS, COSSIN, SIN, M, minimum_axis, AMin, ArMin, CoefAB).Area;
	float A = Loop(n_points, PTRect, PHMaxx, PHMinn, THMaxx, THMinn, theta_cg, station_keeping_error, pointing_error, rotational_error, orbital_position, COSCOS, COSSIN, SIN, M, minimum_axis, AMin, ArMin, CoefAB).A;
	float B = Loop(n_points, PTRect, PHMaxx, PHMinn, THMaxx, THMinn, theta_cg, station_keeping_error, pointing_error, rotational_error, orbital_position, COSCOS, COSSIN, SIN, M, minimum_axis, AMin, ArMin, CoefAB).B;
	float CK = Loop(n_points, PTRect, PHMaxx, PHMinn, THMaxx, THMinn, theta_cg, station_keeping_error, pointing_error, rotational_error, orbital_position, COSCOS, COSSIN, SIN, M, minimum_axis, AMin, ArMin, CoefAB).CK;
	float Theta = Loop(n_points, PTRect, PHMaxx, PHMinn, THMaxx, THMinn, theta_cg, station_keeping_error, pointing_error, rotational_error, orbital_position, COSCOS, COSSIN, SIN, M, minimum_axis, AMin, ArMin, CoefAB).Theta;
	float Phi = Loop(n_points, PTRect, PHMaxx, PHMinn, THMaxx, THMinn, theta_cg, station_keeping_error, pointing_error, rotational_error, orbital_position, COSCOS, COSSIN, SIN, M, minimum_axis, AMin, ArMin, CoefAB).Phi;

	// Call Orient
	float Alpha = Orient(A, B, CK).Alpha;
	float Beta = Orient(A, B, CK).Beta;
	float Omega = Orient(A, B, CK).Omega;


	float SMX = 2.0f * 57.2957795f * std::atan(1.0f / std::sqrt(Alpha));
	float SMN = 2.0f * 57.2957795f * std::atan(1.0f / std::sqrt(Beta));

	float YORI = 90.0f - Omega;

	if (YORI < 0) {
		YORI = 180.0f + YORI;
	}

	if (Phi <= -180) {
		Phi = Phi + 360.0f;
	}
	else if (Phi > 180) {
		Phi = Phi - 360.0f;
	}

	for (int i = 0; i < n_points; i++) {
		float PTXX = Coords_new[i][1];
		if (PTXX <= -180) {
			PTXX = PTXX + 360.0f;
		}
		else if (PTXX > 180) {
			PTXX = PTXX - 360.0f;
		}
	}

	// Call Project
	float** Ang = Project(Theta, Phi, Alpha, Beta, Omega);

	// Call Draw
	float** Ch = Draw(n_points, Theta, Phi, orbital_position, Coords_new, Ang);

	float boresight_lat = Theta;
	float boresight_long = Phi;
	float maj_axis = SMX;
	float minor_axis = SMN;
	float area = Area;
	float orientation = YORI;
	
	Calc_struct output = { boresight_lat, boresight_long, maj_axis, minor_axis, area, orientation };

	return output;
}