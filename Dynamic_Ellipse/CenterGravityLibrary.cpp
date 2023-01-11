#include "pch.h"
#include "CenterGravityLibrary.h"
#include "UpdateLongitudeLibrary.h"
#include <iostream>
#include <algorithm>

// Definition: This library determines the center of gravity (theta and phi)
CenterGravity_struct CenterGravity(const long& n_points, const float& orbital_position, float points_lat[], float points_long[]) {
    // Initialize the center of gravity
    float theta_cg = 0.0f;
    float phi_cg = 0.0f;

    // Initialize min and max for theta and phi
    float theta_max = -1000000.0f;
    float theta_min = 1000000.0f;
    float phi_max = -1000000.0f;
    float phi_min = 1000000.0f;

    // Update longitude of the stations
    points_long = UpdateLongitude(n_points, orbital_position, points_long);

    // Updates the longitudes of the stations (For-loop)
    for (int i = 0; i < n_points; i++) {
        theta_cg += points_lat[i];
        phi_cg += points_long[i];

        theta_max = (std::max)(theta_max, points_lat[i]);
        theta_min = (std::min)(theta_min, points_lat[i]);

        phi_max = (std::max)(phi_max, points_long[i]);
        phi_min = (std::min)(phi_min, points_long[i]);
    }

    // After summing all the latitudes and longitudes, the center of gravity are calculated by averaging
    theta_cg = theta_cg / n_points;
    phi_cg = phi_cg / n_points;

    // Two parameters are returned (theta and phi)
    CenterGravity_struct output = { theta_cg ,phi_cg };
    return output;
}