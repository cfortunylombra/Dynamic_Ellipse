#pragma once

#ifdef PTERRORLIBRARY_EXPORTS
#define PTERRORLIBRARY_API __declspec(dllexport)
#else
#define PTERRORLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: PT7Out; given in a list (float**)
extern "C" PTERRORLIBRARY_API float** PtError(float* PTIn, float& TanPe);