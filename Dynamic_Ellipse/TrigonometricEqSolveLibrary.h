#pragma once

#ifdef TRIGLIBRARY_EXPORTS
#define TRIGLIBRARY_API __declspec(dllexport)
#else
#define TRIGLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: Alpha and THFIN
struct TrigSolve_struct {
    float Alpha;
    float THFIN;
};

extern "C" TRIGLIBRARY_API TrigSolve_struct TrigSolve(float& X1, float& Y1, float& X2, float& Y2, float& TH1, float& TH2, float& AMin);