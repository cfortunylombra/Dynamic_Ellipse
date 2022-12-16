#pragma once

#ifdef MINAXIS1LIBRARY_EXPORTS
#define MINAXIS1LIBRARY_API __declspec(dllexport)
#else
#define MINAXIS1LIBRARY_API __declspec(dllimport)
#endif 

// Define Structure
// Outputs: A0, B0, CK0 and Area0 (ellipse parameters)
struct MinAxis1_struct {
    float A0;
    float B0;
    float CK0;
    float Area0;
};

extern "C" MINAXIS1LIBRARY_API MinAxis1_struct MinAxis1(const long& M, float& A0, float& B0, float& CK0, float& AMin, float** PSOut);