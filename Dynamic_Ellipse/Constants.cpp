#include "pch.h"
#include "Constants.h"
#include <math.h>

// Constants

// Pi
float Pi = (float) atan(1.0f) * 4.0f;
// 1 radian in degrees
float Rad = 180.0f / Pi;
// Upper limit n stations
int NMax = 100;
// Earth Radius
float EarthRadius = 6370997.0f; // m
// GEO Altitude/Earth radius
float GEOAlt_EarthRad_rat = 6.61072f;
