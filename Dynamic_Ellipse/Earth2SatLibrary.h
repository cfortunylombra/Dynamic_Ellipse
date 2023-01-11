#pragma once

#ifdef EARTH2SATLIBRARY_EXPORTS
#define EARTH2SATLIBRARY_API __declspec(dllexport)
#else
#define EARTH2SATLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: Three parameters (PTSat_X, PTSat_Y and PTSat_Z); given in a list (float*)
extern "C" EARTH2SATLIBRARY_API float* Earth2Sat(float** MatrixEC2Sat, float PTEarth[]);