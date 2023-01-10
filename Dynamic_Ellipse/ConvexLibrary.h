#pragma once

#ifdef CONVEXLIBRARY_EXPORTS
#define CONVEXLIBRARY_API __declspec(dllexport)
#else
#define CONVEXLIBRARY_API __declspec(dllimport)
#endif

// Outputs: Coordinates; given as float**
extern "C" CONVEXLIBRARY_API  float** Convex(float** Coords, float& NIn);