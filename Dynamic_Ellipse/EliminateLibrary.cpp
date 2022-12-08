#include "pch.h"
#include "EliminateLibrary.h"
#include <iostream>

// Definition: This library eliminates points which are automatically within the ellipse if other points are within or on the ellipse
float** EliminatePoints(const long& n_points, float** PTSatRect) {
	
	//Initialization
	float Invec[400];
	float RLen[400];

	const long& N6 = n_points;
	for (int i = 0; i < N6; i++) {
		Invec[i] = 1;
	}

	// Get rid of symmetric points
	for (int i = 0; i < N6; i++) {
		RLen[i] = std::sqrt(std::pow(PTSatRect[i][0], 2) + std::pow(PTSatRect[i][1], 2));
	}

	for (int i = 0; i < N6; i++) {
		if (Invec[i] == 0) {
			continue;
		}
		float PT1 = PTSatRect[i][0];
		float PT2 = PTSatRect[i][1];
		for (int j = 0; j < N6; j++) {
			if (j == i || Invec[j] == 0) {
				continue;
			}
			float PX1 = PTSatRect[j][0];
			float PX2 = PTSatRect[j][1];
			if (std::abs(RLen[i]-RLen[j])> 0.0000001){
				continue;
			}
			if ((std::abs(PT1 - PX1) < 0.0000001 && std::abs(PT2 - PX2) < 0.0000001) || (std::abs(PT1 + PX1) < 0.0000001 && std::abs(PT2 + PX2) < 0.0000001)) {
				if (RLen[i] >= RLen[j]) {
					Invec[j] = 0;
				}
				else {
					Invec[i] = 0;
					break;
				}
			}
		}
	}

	// Eliminate superfluous points
	for (int i = 0; i < N6 - 1; i++) {
		if (Invec[i] == 0) {
			continue;
		}
		float X1 = PTSatRect[i][0];
		float Y1 = PTSatRect[i][1];
		for (int j = 1; j < N6; j++) {
			if (Invec[j] == 0) {
				continue;
			}
			float X2 = PTSatRect[j][0];
			float Y2 = PTSatRect[j][1];
			for (int k = 0; k < N6; k++) {
				if ((k == i) || (k == j) || (Invec[k] == 0)) {
					continue;
				}
				float X3 = PTSatRect[k][0];
				float Y3 = PTSatRect[k][1];
				if ((std::abs(X1) < 0.00000001) && (std::abs(X2) < 0.00000001)) {
					if (std::abs(X3) > 0.00000001) {
						continue;
					}
					if ((std::abs(Y3) > std::abs(Y1)) && (std::abs(Y3) > std::abs(Y2))) {
						continue;
					}
					Invec[k] = 0;
				}

				else if (std::abs(X1 - X2) <= 0.00000001) {
					float P2 = (X1 * Y2 - X2 * Y1) / (X1 + X2);
					float M2 = (Y1 + Y2) / (X1 + X2);
					if ((std::abs(Y3 - M2 * X3) <= std::abs(P2)) && (std::abs(X3) <= std::abs(X1))) {
						Invec[k] = 0;
					}
				}

				else if (std::abs(X1 + X2) <= 0.00000001) {
					float P1 = (X2 * Y1 - X1 * Y2) / (X2 - X1);
					float M1 = (Y2 - Y1) / (X2 - X1);
					if ((std::abs(Y3 - M1 * X3) <= std::abs(P1)) && (std::abs(X3) <= std::abs(X1))) {
						Invec[k] = 0;
					}
				}

				else {
					float P1 = (X2 * Y1 - X1 * Y2) / (X2 - X1);
					float M1 = (Y2 - Y1) / (X2 - X1);
					float P2 = (X1 * Y2 - X2 * Y1) / (X1 + X2);
					float M2 = (Y1 + Y2) / (X1 + X2);
					if ((std::abs(Y3 - M1 * X3) <= std::abs(P1)) && (std::abs(Y3 - M2 * X3) <= std::abs(P2))) {
						Invec[k] = 0;
					}
				}
			}
		}
	}

	int InvecCount = 0;
	for (int i = 0; i < N6; i++) {
		if (Invec[i] == 0) {
			continue;
		}
		InvecCount = InvecCount + 1;
	}

	float** PSOut;
	PSOut = new float* [InvecCount];
	for (int i = 0; i < InvecCount; i++) {
		PSOut[i] = new float[3];
	}

	int M = 0;
	for (int i = 0; i < N6; i++) {
		if (Invec[i] == 0) {
			continue;
		}
		PSOut[M][0] = PTSatRect[i][0];
		PSOut[M][1] = PTSatRect[i][1];
		PSOut[M][2] = PTSatRect[i][2];
		M = M + 1;
	}


	// Return: PSOut with three columns (X,Y and Z)
	return PSOut;
}