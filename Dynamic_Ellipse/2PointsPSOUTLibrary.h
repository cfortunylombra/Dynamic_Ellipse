#pragma once

#ifdef PTS2OUTLIBRARY_EXPORTS
#define PTS2OUTLIBRARY_API __declspec(dllexport)
#else
#define PTS2OUTLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: A0, B0, CK0 and Area0
struct TwoPts_struct {
    float A0;
    float B0;
    float CK0;
    float Area0;
};

extern "C" PTS2OUTLIBRARY_API TwoPts_struct TwoPts(const long& M, float** PSOut, float& A0, float& B0, float& CK0, float& CoefAB);