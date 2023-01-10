#pragma once

#ifdef XYPRIMLIBRARY_EXPORTS
#define XYPRIMLIBRARY_API __declspec(dllexport)
#else
#define XYPRIMLIBRARY_API __declspec(dllimport)
#endif 

// Define Structure
// Outputs: X and Y
struct Rotate_struct {
    float X;
    float Y;
};

extern "C" XYPRIMLIBRARY_API Rotate_struct Rotate(float& XPri, float& YPri, float& Omega);