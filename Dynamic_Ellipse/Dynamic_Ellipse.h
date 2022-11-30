// The following ifdef block is the standard way of creating macros which make exporting
// from a DLL simpler. All files within this DLL are compiled with the DYNAMICELLIPSE_EXPORTS
// symbol defined on the command line. This symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see
// DYNAMICELLIPSE_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef DYNAMICELLIPSE_EXPORTS
#define DYNAMICELLIPSE_API __declspec(dllexport)
#else
#define DYNAMICELLIPSE_API __declspec(dllimport)
#endif

// This class is exported from the dll
class DYNAMICELLIPSE_API CDynamicEllipse {
public:
	CDynamicEllipse(void);
	// TODO: add your methods here.
};

extern DYNAMICELLIPSE_API int nDynamicEllipse;

DYNAMICELLIPSE_API int fnDynamicEllipse(void);
