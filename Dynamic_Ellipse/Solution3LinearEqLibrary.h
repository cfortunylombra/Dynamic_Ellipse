#pragma once

#ifdef DETRM3LIBRARY_EXPORTS
#define DETRM3LIBRARY_API __declspec(dllexport)
#else
#define DETRM3LIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: A, B, CK
struct Sol3LinEq_struct {
    float A;
    float B;
    float CK;
};

extern "C" DETRM3LIBRARY_API Sol3LinEq_struct Sol3LinEq(float& X1, float& Y1, float& X2, float& Y2, float& X3, float& Y3);