// Dynamic_Ellipse.cpp : Defines the exported functions for the DLL.
//

#include "pch.h"
#include "framework.h"
#include "Dynamic_Ellipse.h"


// This is an example of an exported variable
DYNAMICELLIPSE_API int nDynamicEllipse=0;

// This is an example of an exported function.
DYNAMICELLIPSE_API int fnDynamicEllipse(void)
{
    return 0;
}

// This is the constructor of a class that has been exported.
CDynamicEllipse::CDynamicEllipse()
{
    return;
}
