#include <intrin.h>

#pragma intrinsic(__rdtsc)

extern "C" __declspec(dllexport) unsigned __int64 __stdcall Rdtsc(void)
{
	return __rdtsc();
}
