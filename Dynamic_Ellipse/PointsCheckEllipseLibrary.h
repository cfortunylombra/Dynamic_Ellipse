#pragma once

#ifdef POINTSCHECKLIBRARY_EXPORTS
#define POINTSCHECKLIBRARY_API __declspec(dllexport)
#else
#define POINTSCHECKLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: A0, B0, CK0 and Area0
struct PointsCheck_struct {
    float A0;
    float B0;
    float CK0;
    float Area0;
};

extern "C" POINTSCHECKLIBRARY_API PointsCheck_struct PointsCheck(const long& M, float& AMin, float& CoefAB, float& ArMin, float& A0, float& B0, float& CK0, float** PSOut);