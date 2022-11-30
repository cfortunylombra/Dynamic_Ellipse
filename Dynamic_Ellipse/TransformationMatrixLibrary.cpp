#include "pch.h"
#include "TransformationMatrixLibrary.h"
#include "Constants.h"
#include <iostream>
#include <cmath>

// Definition: This library determines the transformation matrix (Satellocentric to Earth Center)
TransformationMatrix_struct TransformationMatrix(const float& orbital_position, const float& BorLat, const float& BorLong) {
	
	// Change Boresight
	float ThetaBor = BorLat / Rad;
	float PhiBor = (BorLong-orbital_position) / Rad;

	// Calculate the EC coordinate of the sat as seen from the boresight
	float BS_1 = GEOAlt_EarthRad_rat - std::cos(ThetaBor) * std::cos(PhiBor);
	float BS_2 = -std::cos(ThetaBor) * std::sin(PhiBor);
	float BS_3 = -std::sin(ThetaBor);

	// Calculate the distance from boresight to satellite
	float BS_Dist = std::sqrt(std::pow(BS_1,2)+std::pow(BS_2,2)+std::pow(BS_3,2));

	// Calculate the unit vector in the Za direction, in EC coordinates
	float Za_1 = -BS_1 / BS_Dist;
	float Za_2 = -BS_2 / BS_Dist;
	float Za_3 = -BS_3 / BS_Dist;

	// Calculate the unit vector in the Ya direction, in EC coordinates
	float ZaZg_cross = std::sqrt(std::pow(Za_2, 2) + std::pow(Za_2, 1));

	float Ya_1 = Za_2 / ZaZg_cross;
	float Ya_2 = -Za_1 / ZaZg_cross;
	float Ya_3 = 0.0f;

	// Calculate the unit vector in the Xa direction, in EC coordinates
	float Xa_1 = Ya_2 * Za_3;
	float Xa_2 = -Ya_1 * Za_3;
	float Xa_3 = Ya_1 * Za_2 - Ya_2 * Za_1;

	// Determine transformation matrix from Sat to EC system
	float** MatrixSat2EC;
	MatrixSat2EC = new float* [3];
	for (int i=0; i < 3; i++) {
		MatrixSat2EC[i] = new float[3];
	}

	MatrixSat2EC[0][0] = Xa_1;
	MatrixSat2EC[1][0] = Xa_2;
	MatrixSat2EC[2][0] = Xa_3;
	MatrixSat2EC[0][1] = Ya_1;
	MatrixSat2EC[1][1] = Ya_2;
	MatrixSat2EC[2][1] = Ya_3;
	MatrixSat2EC[0][2] = Za_1;
	MatrixSat2EC[1][2] = Za_2;
	MatrixSat2EC[2][2] = Za_3;

	// Determine transformation matrix from Sat to EC system
	float** MatrixEC2Sat;
	MatrixEC2Sat = new float* [3];
	for (int i=0; i < 3; i++) {
		MatrixEC2Sat[i] = new float[3];
	}

	MatrixEC2Sat[0][0] = Xa_1;
	MatrixEC2Sat[0][1] = Xa_2;
	MatrixEC2Sat[0][2] = Xa_3;
	MatrixEC2Sat[1][0] = Ya_1;
	MatrixEC2Sat[1][1] = Ya_2;
	MatrixEC2Sat[1][2] = Ya_3;
	MatrixEC2Sat[2][0] = Za_1;
	MatrixEC2Sat[2][1] = Za_2;
	MatrixEC2Sat[2][2] = Za_3;

	// A matrix is returned
	TransformationMatrix_struct output = {MatrixSat2EC, MatrixEC2Sat};
	return output;
}