#include "pch.h"
#include "XPrimeYPrime2XYLibrary.h"
#include "Constants.h"
#include <iostream>
#include <cmath>

// Definition: This library converts x and y prime coordinates to x and y coordinates
Rotate_struct Rotate(float& XPri, float& YPri, float& Omega) {
	float Omeg = Omega / Rad;

	float X = XPri * std::cos(Omeg) - YPri * std::sin(Omeg);
	float Y = XPri * std::sin(Omeg) - YPri * std::cos(Omeg);

	Rotate_struct output = { X,Y };
	return output;
}