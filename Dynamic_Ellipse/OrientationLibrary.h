#pragma once

#ifdef ORIENTATIONLIBRARY_EXPORTS
#define ORIENTATIONLIBRARY_API __declspec(dllexport)
#else
#define ORIENTATIONLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: Alpha, Beta, and Omega
struct Orient_struct {
	float Alpha;
	float Beta;
	float Omega;
};

extern "C" ORIENTATIONLIBRARY_API Orient_struct Orient(float& A, float& B, float& CK);