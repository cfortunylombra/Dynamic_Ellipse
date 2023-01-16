#pragma once

#ifdef LOOP1LIBRARY_EXPORTS
#define LOOP1LIBRARY_API __declspec(dllexport)
#else
#define LOOP1LIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: 
struct Loop1_struct {
	float Area;
	float A;
	float B;
	float CK;
	float Theta;
	float Phi;
	float Dango;
};

extern "C" LOOP1LIBRARY_API Loop1_struct Loop1(float& NTot, float& THct, float& PHct, float& dango, float** PTRect);