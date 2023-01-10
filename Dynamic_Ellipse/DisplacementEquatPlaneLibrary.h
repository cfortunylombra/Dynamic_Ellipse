#pragma once

#ifdef DISPLIBRARY_EXPORTS
#define DISPLIBRARY_API __declspec(dllexport)
#else
#define DISPLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: Two vectors (ZVec and YVec)
struct TransXYZ_struct {
    float* ZVec;
    float* YVec;
};

extern "C" DISPLIBRARY_API TransXYZ_struct TransXYZ(float** MGTOS);