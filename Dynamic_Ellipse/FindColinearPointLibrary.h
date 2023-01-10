#pragma once

#ifdef BETWLIBRARY_EXPORTS
#define BETWBRARY_API __declspec(dllexport)
#else
#define BETWLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: a number to determine which of the three points is in between
extern "C" BETWLIBRARY_API int Betw(float& X1, float& Y1, float& X2, float& Y2, float& X3, float& Y3);