#pragma once

#ifdef ROTERRORLIBRARY_EXPORTS
#define ROTERRORLIBRARY_API __declspec(dllexport)
#else
#define ROTERRORLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: PTSOut; given in a list (float**)
extern "C" ROTERRORLIBRARY_API float** RotError(float* PTIn);