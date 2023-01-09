#pragma once

#ifdef ADDPOINTROTLIBRARY_EXPORTS
#define ADDPOINTROTLIBRARY_API __declspec(dllexport)
#else
#define ADDPOINTROTLIBRARY_API __declspec(dllimport)
#endif

// Outputs: PXRect as a matrix
extern "C" ADDPOINTROTLIBRARY_API float** Sol3LinEq(float& N, float** PSRect, float& AngDif, float& MX);