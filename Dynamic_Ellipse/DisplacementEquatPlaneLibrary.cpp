#include "pch.h"
#include "DisplacementEquatPlaneLibrary.h"
#include "Constants.h"
#include <iostream>
#include <cmath>

// Definition: This library calculates the components of a 0.1 deg displacement in the Equatorial plane in the S-C coordinate system
TransXYZ_struct TransXYZ(float** MGTOS) {
	float* ZVec = new float[3];
	float* YVec = new float[3];

	float Dist = GEOAlt_EarthRad_rat * TanSk_rad;

	ZVec[0] = Dist * MGTOS[0][2];
	ZVec[1] = Dist * MGTOS[1][2];
	ZVec[2] = Dist * MGTOS[2][2];

	YVec[0] = Dist * MGTOS[0][1];
	YVec[1] = Dist * MGTOS[1][1];
	YVec[2] = Dist * MGTOS[2][1];

	TransXYZ_struct output = { ZVec, YVec };
	return output;
}