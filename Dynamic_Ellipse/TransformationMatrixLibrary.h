#pragma once

#ifdef TRANSLIBRARY_EXPORTS
#define TRANSLIBRARY_API __declspec(dllexport)
#else
#define TRANSLIBRARY_API __declspec(dllimport)
#endif

// Define Structure
// Outputs: Two matrices (1st Sat to Earth Centered; 2nd Earth Centered to Sat (inverse/orthogonal of 1st))
struct TransformationMatrix_struct {
    float** MatrixSat2EC;
    float** MatrixEC2Sat;
};

extern "C" TRANSLIBRARY_API TransformationMatrix_struct TransformationMatrix(const float& orbital_position, const float& BorLat, const float& BorLong);