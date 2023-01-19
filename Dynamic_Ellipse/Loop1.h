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

extern "C" LOOP1LIBRARY_API Loop1_struct Loop1(float& NTot, float& THct, float& PHct, float& dango, float& station_keeping_error, float& pointing_error, float& rotational_error, float& orbital_position, float** PTRect, float** COSCOS, float** COSSIN, float** SIN, float& A, float& B, float& CK, float& M, float& XMin, float& AMin, float& ArMin, float& CoefAB);