#pragma once

#ifdef LOOPLIBRARY_EXPORTS
#define LOOPLIBRARY_API __declspec(dllexport)
#else
#define LOOPLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: 
struct Loop_struct {
	float Area;
	float A;
	float B;
	float CK;
	float Theta; 
	float Phi;
};

extern "C" LOOPLIBRARY_API Loop_struct Loop(float& NTot, float** PTRect);