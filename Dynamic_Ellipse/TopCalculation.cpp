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
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: Merge of all the libraries
Calc_struct Calc(long& status, const float& pointing_error, const float& rotational_error, const float& station_keeping_error, const float& minimum_axis, float& orbital_position, float& n_points, float points_lat[], float points_long[], float** COSCOS, float** COSSIN, float** SIN) {
	
	// Call CenterGravity
	float theta_cg = CenterGravity(n_points, orbital_position, points_lat, points_long).theta_cg;
	float phi_cg = CenterGravity(n_points, orbital_position, points_lat, points_long).phi_cg;

	// Call UpdateLongitude
	float* points_long = UpdateLongitude(n_points, orbital_position, points_long);

	float NT = n_points;

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
				Dist2 = (std::max)(Dist2, std::pow(XOut - GEOAlt_EarthRad_rat * COSCOS[ki][0], 2) + std::pow(YOut - GEOAlt_EarthRad_rat * COSSIN[ki][0], 2) + std::pow(ZOut - GEOAlt_EarthRad_rat * SIN[ki][0], 2));
			}

			for (int ki = 0; ki < 4; ki++) {
				Dist2 = (std::max)(Dist2, std::pow(XOut - GEOAlt_EarthRad_rat * COSCOS[ki][1], 2) + std::pow(YOut - GEOAlt_EarthRad_rat * COSSIN[ki][1], 2) + std::pow(ZOut - GEOAlt_EarthRad_rat * SIN[ki][1], 2));
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
		float PTXX = PTGEO[i][1];
		if (PTXX <= -180) {
			PTXX = PTXX + 360.0f;
		}
		else if (PTXX > 180) {
			PTXX = PTXX - 360.0f;
		}
	}

	// Call Project
	float** Ang = Project(Theta, Phi, Alpha, Beta, Omega);

	if (NFLAG == 1) {
		n_points = 2;
	}

	// Call Draw
	float** Ch = Draw(n_points, Theta, Phi, orbital_position, PTGeo, Ang);
	
	Calc_struct output = { boresight_lat, boresight_long, maj_axis, minor_axis, area, orientation };

	return output;
}