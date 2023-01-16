#include "pch.h"
#include "ConvexLibrary.h"
#include "FindColinearPointLibrary.h"
#include <iostream>
#include <algorithm>

// Definition: This library deletes points from an arbitrary set of points to produce a convex set
Convex_struct Convex(float** Coords, float& NIn) {
	float EPS = 0.0001;

	float* IamOut = new float[200];

	for (int i = 0; i < NIn; i++) {
		IamOut[i] = 0.0f;
	}

	loop_500:
	for (int i = 0; i < NIn - 2; i++) {
		if (IamOut[i] == 1) {
			goto loop_500;
		}
		float X1 = Coords[i][0];
		float Y1 = Coords[i][1];

		loop_400:
		for (int j = i + 1; j < NIn - 1; j++) {
			if (IamOut[j] == 1) {
				goto loop_400;
			}

			float X2 = Coords[j][0];
			float Y2 = Coords[j][1];
			float A12 = Y2 - Y1;
			float B12 = X1 - X2;
			float C12 = - X1 * A12 - Y1 * B12;

			loop_300:
			for (int k = j + 1; k < NIn; k++) {
				if (IamOut[k] == 1) {
					goto loop_300;
				}
				
				float X3 = Coords[k][0];
				float Y3 = Coords[k][1];

				float A23 = Y3 - Y2;
				float B23 = X2 - X3;
				float C23 = -X2 * A23 - Y2 * B23;

				float A31 = Y1 - Y3;
				float B31 = X3 - X1;
				float C31 = -X3 * A31 - Y3 * B31;

				float V1 = A23 * X1 + B23 * Y1 + C23;
				float V2 = A31 * X2 + B31 * Y2 + C31;
				float V3 = A12 * X3 + B12 * Y3 + C12;

				if ((std::abs(V1) <= EPS) || (std::abs(V2) <= EPS) || (std::abs(V3) <= EPS)) {
					// Call Between
					int Num = Betw(X1, Y1, X2, Y2, X3, Y3);

					if (Num == 1) {
						IamOut[i] = 1;
						goto loop_500;
					}

					else if (Num == 2) {
						IamOut[j] = 1;
						goto loop_400;
					}

					else if (Num == 3) {
						IamOut[k] = 1;
						goto loop_300;
					}
				}

				loop_200:
				for (int L = 0; L < NIn; L++) {
					if ((L == i) || (L == j) || (L == k) || (IamOut[L] == 1)) {
						goto loop_200;
					}

					float Xp = Coords[L][0];
					float Yp = Coords[L][1];

					float Vp = A23 * Xp + B23 * Yp + C23;

					// Check Coinearity
					if (std::abs(Vp) <= EPS) {
						int Num = Betw(Xp, Yp, X2, Y2, X3, Y3);

						if (Num == 1) {
							IamOut[L] = 1;
							goto loop_200;
						}

						else if (Num == 2) {
							IamOut[j] = 1;
							goto loop_400;
						}

						else if (Num == 3) {
							IamOut[k] = 1;
							goto loop_300;
						}
					}

					if (Vp * V1 < 0) {
						goto loop_200;
					}

					Vp = A31 * Xp + B31 * Yp + C31;

					// Check Coinearity
					if (std::abs(Vp) <= EPS) {
						int Num = Betw(X1, Y1, Xp, Yp, X3, Y3);

						if (Num == 1) {
							IamOut[i] = 1;
							goto loop_500;
						}

						else if (Num == 2) {
							IamOut[L] = 1;
							goto loop_200;
						}

						else if (Num == 3) {
							IamOut[k] = 1;
							goto loop_300;
						}
					}

					if (Vp * V2 < 0) {
						goto loop_200;
					}

					Vp = A12 * Xp + B12 * Yp + C12;

					// Check Coinearity
					if (std::abs(Vp) <= EPS) {
						int Num = Betw(X1, Y1, X2, Y2, Xp, Yp);

						if (Num == 1) {
							IamOut[i] = 1;
							goto loop_500;
						}

						else if (Num == 2) {
							IamOut[j] = 1;
							goto loop_400;
						}

						else if (Num == 3) {
							IamOut[L] = 1;
							goto loop_200;
						}
					}

					if (Vp * V3 < 0) {
						goto loop_200;
					}

					IamOut[L] = 1;
				}
			}
		}
	}

	int NOut = 0;
	loop_1000:
	for (int i = 0; i < NIn; i++) {
		if (IamOut[i] == 1) {
			goto loop_1000;
		}
		NOut = NOut + 1;
	}

	float** Coords_new;
	Coords_new = new float* [NOut];
	for (int i = 0; i < NOut; i++) {
		Coords[i] = new float[2];
	}

	int iter = 0;
	loop_1001:
	for (int i = 0; i < NIn; i++) {
		if (IamOut[i] == 1) {
			goto loop_1001;
		}
		Coords_new[iter][0] = Coords[i][0];
		Coords_new[iter][1] = Coords[i][1];
		iter = iter + 1;
	}

	Convex_struct output = { NOut, Coords_new };
	return output;
}