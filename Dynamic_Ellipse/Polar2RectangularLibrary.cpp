#include "pch.h"
#include "Polar2RectangularLibrary.h"
#include "Constants.h"
#include <iostream>

// Definition: This library changes GEO polar coordinates into E-C rectangular coordinates on unit earth
Pol2Rect_struct Pol2Rect(const float& Thetain, const float& Phiin){

	// Change from DEG to RAD
	float ThetainRad = Thetain / Rad;
	float PhiinRad = Thetain / Rad;

	// Conversion
	float Xout = std::cos(ThetainRad) * std::cos(PhiinRad);
	float Yout = std::cos(ThetainRad) * std::sin(PhiinRad);
	float Zout = std::sin(ThetainRad);

	// Three parameters are returned (Xout, Yout and Zout)
	Pol2Rect_struct output = { Xout ,Yout, Zout };
	return output;
}