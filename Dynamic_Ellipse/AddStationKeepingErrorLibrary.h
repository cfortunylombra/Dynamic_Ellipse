#pragma once

#ifdef SKERRORLIBRARY_EXPORTS
#define SKERRORLIBRARY_API __declspec(dllexport)
#else
#define SKERRORLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: PTOut; given in a list (float*)
extern "C" SKERRORLIBRARY_API float* SkError(float* PTIn, float& DifAng);