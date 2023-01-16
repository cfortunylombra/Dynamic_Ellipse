#pragma once

#ifdef CALCLIBRARY_EXPORTS
#define CALCLIBRARY_API __declspec(dllexport)
#else
#define CALCLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: 
struct Calc_struct {
    float boresight_lat;
    float boresight_long;
    float maj_axis;
    float minor_axis;
    float area;
    float orientation;
};

extern "C" CALCLIBRARY_API Calc_struct Calc(long& status, const float& pointing_error, const float& rotational_error, const float& station_keeping_error, const float& minimum_axis, float& orbital_position, float& n_points, float points_lat[], float points_long[], float** COSCOS, float** COSSIN, float** SIN);