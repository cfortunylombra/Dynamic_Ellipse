#pragma once

#ifdef EDGELIBRARY_EXPORTS
#define EDGELIBRARY_API __declspec(dllexport)
#else
#define EDGELIBRARY_API __declspec(dllimport)
#endif 

// Outputs: Ellipse coordinates as a float**
extern "C" EDGELIBRARY_API float** ElPoly(float& Alpha, float& Beta);