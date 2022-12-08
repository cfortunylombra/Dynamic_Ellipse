#pragma once

#ifdef DELETELIBRARY_EXPORTS
#define DELETELIBRARY_API __declspec(dllexport)
#else
#define DELETELIBRARY_API __declspec(dllimport)
#endif

// Outputs: Outer points of the ellipse in the form of a matrix (rows: n stations; cols: 3)
extern "C" DELETELIBRARY_API float** EliminatePoints(const long& n_points,float** PTSatRect);