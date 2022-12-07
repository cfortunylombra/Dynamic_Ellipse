#pragma once

#ifdef NORMLIBRARY_EXPORTS
#define NORMLIBRARY_API __declspec(dllexport)
#else
#define NORMLIBRARY_API __declspec(dllimport)
#endif

// Outputs: Satellite centered coordinates normalized (PTSatNormZ_X, PTSatNormZ_Y and PTSatNormZ_Z)
extern "C" NORMLIBRARY_API float* NormVector(float PTSat[]);