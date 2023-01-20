#pragma once

#ifdef PROJLIBRARY_EXPORTS
#define PROJLIBRARY_API __declspec(dllexport)
#else
#define PROJLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: Ang in terms of float*
extern "C" PROJLIBRARY_API float** Proj(float& Theta, float& Phi, float& Alpha, float& Beta, float& Omega, float& SatLon, float* PtSat);