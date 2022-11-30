#include "pch.h"
#include "UpdateLongitudeLibrary.h"
#include <iostream>
#include <algorithm>

// Definition: This library computes the new longitude of the stations
float* UpdateLongitude(const long& n_points, const float& orbital_position, float points_long[]) {
    // Updates the longitudes of the stations
    for (int i = 0; i < n_points; i++) {
        float DeltaPhi = points_long[i] - orbital_position;
        if (DeltaPhi <= -180) {
            points_long[i] = points_long[i] + 360;
        }
        else if (DeltaPhi > 180) {
            points_long[i] = points_long[i] - 360;
        }
    }
	return (float*) points_long;
}