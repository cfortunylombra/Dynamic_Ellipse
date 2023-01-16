#pragma once

#ifdef CONVEXLIBRARY_EXPORTS
#define CONVEXLIBRARY_API __declspec(dllexport)
#else
#define CONVEXLIBRARY_API __declspec(dllimport)
#endif

// Structure
// Outputs: NOuts (int) and Coordinates; given as float**
struct Convex_struct {
	int NOut;
	float** Coords_new;
};

extern "C" CONVEXLIBRARY_API Convex_struct Convex(float** Coords, float& NIn);