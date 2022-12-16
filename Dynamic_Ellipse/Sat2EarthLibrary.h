#pragma once

#ifdef SAT2EARTHLIBRARY_EXPORTS
#define SAT2EARTHLIBRARY_API __declspec(dllexport)
#else
#define SAT2EARTHLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: Three parameters (PTEarth_X, PTEarth_Y and PTEarth_Z); given in a list (float*)
extern "C" SAT2EARTHLIBRARY_API float* Sat2Earth(float** MatrixSat2EC, float PTSat[]);