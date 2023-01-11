#pragma once

#ifdef ANGSKDIFFLIBRARY_EXPORTS
#define ANGSKDIFFLIBRARY_API __declspec(dllexport)
#else
#define ANGSKDIFFLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: AngDif; given in a list (float*)
extern "C" ANGSKDIFFLIBRARY_API float* StaKep(float& ThC, float& PhC, float& N, float** PtRect, float** COSCOS, float** COSSIN, float** SIN);