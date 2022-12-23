// Console_Ellipse.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "Constants.h"
#include "CenterGravityLibrary.h"
#include "UpdateLongitudeLibrary.h"
#include "Polar2RectangularLibrary.h"
#include "TransformationMatrixLibrary.h"
#include "Earth2SatLibrary.h"
#include "Sat2EarthLibrary.h"
#include "NormVectorLibrary.h"
#include "EliminateLibrary.h"
#include "PointsCheckEllipseLibrary.h"
#include "MinimumSemiMinorAxisLibrary.h"
#include "MinimumSemiMinorAxis2Library.h"
#include "TrigonometricEqSolveLibrary.h"
#include "3PointsEllipseLibrary.h"
#include "Solution3LinearEqLibrary.h"
#include "2PointsPSOUTLibrary.h"
#include "DeterminantLibrary.h"
#include "OrientationLibrary.h"
#include <iostream>
#include <algorithm>

void MinimumEllipseCalculation(
    long& status,
    const float& pointing_error,
    const float& rotational_error,
    const float& station_keeping_error,
    const float& minimum_axis,
    const float& orbital_position,
    const long& n_points,
    float points_lat[],
    float points_long[]) {
    return;
    /* This function needs to output the following variables :
     1.float& boresight_lat,
     2.float& boresight_long,
     3.float& maj_axis,
     4.float& minor_axis,
     5.float& area,
     6.float& orientation
    */
}

int main()
{
    std::cout << "Hello World!\n";
    /* Status: 0 - program worked OK; valid results
    1 - invalid value of n_points(n_points <= 2 or > 100)
    2 - invalid orbital position(orbital_position < -180.0 or orbital_position > 180.0)
    3 - invalid value of pointing error(pointing_error < 0.0 or pointing_error > 1.0)
    4 - invalid value of rotation error(rotational_error < 0.0 or rotational_error > 3.0)
    5 – not used
    6 - invalid value of minimal value of axis(minimum_axis < 0.0)
    7 - some points are not visible from the satellite.
    8 – others – undefined*/
    long status = 0;

    // Pointing error in degrees (Invalid if <0 or >1)
    float perr = 0.0f;

    // Rotational error in degrees (Invalid if <0 or >3)
    float rerr = 0.0f;

    // Station keeping error in degrees
    float skerr = 0.0f;

    // Minimum axis in degrees 
    float minbeamw = 0.0f;

    // Orbital position in degrees (Invalid <-180 or ?180)
    float orbpos = 0.0f;

    // Number of ground stations (Invalid <=2 or >100)
    const int npts = 1;

    //  Latitude of ground stations in degrees (Invalid if size is different to npts)
    float tlat[npts];

    // Longitude of ground stations in degrees (Invalid if size is different to npts)
    float tlon[npts];

    // Assigning longitudes and latitudes
    for (int i = 0; i < npts; ++i)
    {
        tlat[i] = 0.0f;
        tlon[i] = 0.0f;
    }

    MinimumEllipseCalculation(status, perr, rerr, skerr, minbeamw, orbpos, npts,
        tlat, tlon);

    if (0 == status)
    {
    }
    else
    {
        throw ("Ellipse parameters recalculation failed");
    }

    // Test: Center of Gravity
    std::cout << "\nTest 1:" << std::endl;
    const int npts_test1 = 4;
    float orbpos_test1 = 0.0f;
    float tlat_test1[npts_test1];
    float tlon_test1[npts_test1];
    tlat_test1[0] = -25.0f;
    tlon_test1[0] = -25.0f;
    tlat_test1[1] = 25.0f;
    tlon_test1[1] = -25.0f;
    tlat_test1[2] = -25.0f;
    tlon_test1[2] = 225.0f;
    tlat_test1[3] = 30.0f;
    tlon_test1[3] = 25.0f;
    CenterGravity_struct theta_phi_cg = CenterGravity(npts_test1, orbpos_test1, tlat_test1, tlon_test1);
    std::cout << "Theta_cg:" << theta_phi_cg.theta_cg << std::endl;
    std::cout << "Phi_cg:" << theta_phi_cg.phi_cg << std::endl;

    // Test: Update Logitudes (Trying with 225deg)
    std::cout << "\nTest 2:" << std::endl;
    std::cout << "Previous longitude in deg of St2:" << tlon_test1[2] << std::endl;
    std::cout << "New longitude in deg of St2:" << UpdateLongitude(npts_test1, orbpos_test1, tlon_test1)[2] << std::endl;

    // Test: Change of coordinates
    std::cout << "\nTest 3:" << std::endl;
    Pol2Rect_struct example = Pol2Rect(35.0, 20.0);
    std::cout << "Xout:" << example.Xout << std::endl;

    // Test: Transformation Matrix
    std::cout << "\nTest 4:" << std::endl;
    TransformationMatrix_struct Matrices = TransformationMatrix(2.0f, 25.0f, 25.0f);
    std::cout << "MatrixSat2EC[1][1]:" << Matrices.MatrixSat2EC[1][1] << std::endl;

    //Test: Earth 2 Sat coordinates
    std::cout << "\nTest 5:" << std::endl;
    float PTEarth[3];
    PTEarth[0] = EarthRadius;
    PTEarth[1] = 0.0f;
    PTEarth[2] = 0.0f;
    std::cout << "PTSat[0]:" << Earth2Sat(Matrices.MatrixEC2Sat, PTEarth)[0] << std::endl;

    //Test: Sat 2 Earth coordinates
    std::cout << "\nTest 6:" << std::endl;
    float PTSat[3];
    PTSat[0] = EarthRadius * GEOAlt_EarthRad_rat;
    PTSat[1] = 0.0f;
    PTSat[2] = 0.0f;
    std::cout << "PTEarth[0]:" << Sat2Earth(Matrices.MatrixSat2EC, PTSat)[0] << std::endl;

    //Test: Norm Vector to have a unit z-component
    std::cout << "\nTest 7:" << std::endl;
    float PTSat_Z[3];
    PTSat_Z[0] = 2*EarthRadius;
    PTSat_Z[1] = 3*EarthRadius;
    PTSat_Z[2] = EarthRadius;
    std::cout << "PTSAT_norm[0]:" << NormVector(PTSat_Z)[1] << std::endl;

    //Test: Eliminate points which are automatically within the ellipse if other points are within or on the ellipse
    std::cout << "\nTest 8:" << std::endl;
    float** PTSatRect_test1;
    PTSatRect_test1 = new float* [400];
    for (int i = 0; i < 400; i++) {
        PTSatRect_test1[i] = new float[3];
    }
    PTSatRect_test1[0][0] = EarthRadius;
    PTSatRect_test1[0][1] = 0.0f;
    PTSatRect_test1[0][2] = 0.0f;
    PTSatRect_test1[1][0] = 0.0f;
    PTSatRect_test1[1][1] = EarthRadius;
    PTSatRect_test1[1][2] = 0.0f;
    PTSatRect_test1[2][0] = 0.0f;
    PTSatRect_test1[2][1] = 0.0f;
    PTSatRect_test1[2][2] = EarthRadius;
    PTSatRect_test1[3][0] = EarthRadius / std::sqrt(3.0f);
    PTSatRect_test1[3][1] = EarthRadius / std::sqrt(3.0f);
    PTSatRect_test1[3][2] = EarthRadius / std::sqrt(3.0f);
    std::cout << "PTOut[0]:" << EliminatePoints(npts_test1,PTSatRect_test1)[2][2] << std::endl; // if you try [3][2] gives error!

    //Test: PointsCheckEllipseLibrary
    std::cout << "\nTest 9:" << std::endl;

    //Test: MinimumSemiMinorAxisLibrary
    std::cout << "\nTest 10:" << std::endl;

    //Test: MinimumSemiMinorAxis2Library
    std::cout << "\nTest 11:" << std::endl;

    //Test: TrigonometricEqSolveLibrary
    std::cout << "\nTest 12:" << std::endl;

    //Test: 3PointsEllipseLibrary
    std::cout << "\nTest 13:" << std::endl;

    //Test: Solution3LinearEqLibrary
    std::cout << "\nTest 14:" << std::endl;

    //Test: 2PointsPSOUTLibrary
    std::cout << "\nTest 15:" << std::endl;

    //Test: DeterminantLibrary
    std::cout << "\nTest 16:" << std::endl;

    //Test: OrientationLibrary
    std::cout << "\nTest 17:" << std::endl;

}