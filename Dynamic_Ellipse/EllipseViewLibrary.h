#pragma once

#ifdef DRAWLIBRARY_EXPORTS
#define DRAWLIBRARY_API __declspec(dllexport)
#else
#define DRAWLIBRARY_API __declspec(dllimport)
#endif

// Outputs: CH; given in a list (float**)
extern "C" DRAWLIBRARY_API float** DrawEllipse(float& N, float& Theta, float& Phi, float& SatLon, float** PtsGeo, float** Ang);