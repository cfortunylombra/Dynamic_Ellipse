#pragma once

#ifdef ADDPOINTROTLIBRARY_EXPORTS
#define ADDPOINTROTLIBRARY_API __declspec(dllexport)
#else
#define ADDPOINTROTLIBRARY_API __declspec(dllimport)
#endif

// Outputs: PXRect as a matrix
extern "C" ADDPOINTROTLIBRARY_API float** XTNDEL(float& N, float** PSRect, float* AngDif, float& XMin, float& XPtErr, float& TanPe, float& CROT, float& SROT, float& iSKErr, float& iPTErr, float& iRotErr);