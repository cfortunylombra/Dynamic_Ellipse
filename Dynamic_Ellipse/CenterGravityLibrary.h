#pragma once

#ifdef CGLIBRARY_EXPORTS
#define CGLIBRARY_API __declspec(dllexport)
#else
#define CGLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: theta_cg and phi_cg (center of gravity)
struct CenterGravity_struct {
    float theta_cg;
    float phi_cg;
};

extern "C" CGLIBRARY_API CenterGravity_struct CenterGravity(const long& n_points, const float& orbital_position, float points_lat[], float points_long[]);