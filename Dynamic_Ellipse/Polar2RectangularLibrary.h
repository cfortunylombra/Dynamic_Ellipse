#pragma once

#ifdef POL2RECTLIBRARY_EXPORTS
#define POL2RECTLIBRARY_API __declspec(dllexport)
#else
#define POL2RECTLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: Xout, Yout and Zout (Rectangular coordinates)
struct Pol2Rect_struct {
    float Xout;
    float Yout;
    float Zout;
};

extern "C" POL2RECTLIBRARY_API Pol2Rect_struct Pol2Rect(const float& Thetain, const float& Phiin);