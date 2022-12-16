#pragma once

#ifdef CONSTLIBRARY_EXPORTS
#define CONSTLIBRARY_API __declspec(dllexport)
#else
#define CONSTLIBRARY_API __declspec(dllimport)
#endif

extern "C" CONSTLIBRARY_API float Pi; //Pi
extern "C" CONSTLIBRARY_API float Rad; //Radian
extern "C" CONSTLIBRARY_API int NMax; //Upper limit number of stations
extern "C" CONSTLIBRARY_API float EarthRadius; //Earth radius in m
extern "C" CONSTLIBRARY_API float GEOAlt_EarthRad_rat; //GEO altitude/Earth radius

