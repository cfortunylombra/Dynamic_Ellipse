#pragma once

#ifdef MINAXIS2LIBRARY_EXPORTS
#define MINAXIS2LIBRARY_API __declspec(dllexport)
#else
#define MINAXIS2LIBRARY_API __declspec(dllimport)
#endif 

// Define Structure
// Outputs: A0, B0, CK0 and Area0
struct MinAxis2_struct {
    float A0;
    float B0;
    float CK0;
    float Area0;
};

extern "C" MINAXIS2LIBRARY_API MinAxis2_struct MinAxis2(const long& M, float& A0, float& B0, float& CK0, float& AMin, float** PSOut);