#pragma once

#ifdef DETLIBRARY_EXPORTS
#define DETLIBRARY_API __declspec(dllexport)
#else
#define DETLIBRARY_API __declspec(dllimport)
#endif 

// Define Structure
// Outputs: A, B, CK (ellipse parameters)
struct Det_struct {
    float A;
    float B;
    float CK;
};

extern "C" DETLIBRARY_API Det_struct Det(float& X1, float& Y1, float& X2, float& Y2);