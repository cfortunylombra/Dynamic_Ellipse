#pragma once

#ifdef PTS3LIBRARY_EXPORTS
#define PTS3LIBRARY_API __declspec(dllexport)
#else
#define PTS3LIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: A0, B0, CK0 and Area0
struct Points3Ellipse_struct {
    float A0;
    float B0;
    float CK0;
    float Area0;
};

extern "C" PTS3LIBRARY_API Points3Ellipse_struct Points3Ellipse(const long& M, float** PSOut, float& A0, float& B0, float& CK0, float& CoefAB);