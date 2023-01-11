#include "pch.h"
#include "FindColinearPointLibrary.h"
#include <algorithm>
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

// Definition: This library determines which one of three colinear points lies between the other two
int Betw(float& X1, float& Y1, float& X2, float& Y2, float& X3, float& Y3) {
	float D12 = std::pow(X1 - X2, 2) + std::pow(Y1 - Y2, 2);
	float D13 = std::pow(X1 - X3, 2) + std::pow(Y1 - Y3, 2);
	float D23 = std::pow(X2 - X3, 2) + std::pow(Y2 - Y3, 2);

	float DM = (std::max)((std::max)(D12, D13), D23);

	int Num = 0;

	if (DM == D12) {
		Num = 3;
	}
	else if (DM == D13) {
		Num = 2;
	}
	else {
		Num = 1;
	}

	return Num;
}