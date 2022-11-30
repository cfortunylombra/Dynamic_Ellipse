#pragma once

#ifdef UPDATELONGLIBRARY_EXPORTS
#define UPDATELONGLIBRARY_API __declspec(dllexport)
#else
#define UPDATELONGLIBRARY_API __declspec(dllimport)
#endif 

// Outputs: Float array with longitudes of the stations
extern "C" UPDATELONGLIBRARY_API float* UpdateLongitude(const long& n_points, const float& orbital_position, float points_long[]);