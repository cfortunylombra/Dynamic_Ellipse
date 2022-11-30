#pragma once

#ifdef CONSTLIBRARY_EXPORTS
#define CONSTLIBRARY_API __declspec(dllexport)
#else
#define CONSTLIBRARY_API __declspec(dllimport)
#endif

extern "C" CONSTLIBRARY_API float Pi;
extern "C" CONSTLIBRARY_API float Rad;
extern "C" CONSTLIBRARY_API int NMax;
extern "C" CONSTLIBRARY_API float EarthRadius;
extern "C" CONSTLIBRARY_API float GEOAlt_EarthRad_rat;

