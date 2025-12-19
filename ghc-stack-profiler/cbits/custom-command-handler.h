#ifndef CUSTOM_COMMAND_HANDLER_H
#define CUSTOM_COMMAND_HANDLER_H

#include <stdint.h>
#include <Rts.h>

#define CUSTOM_COMMAND_NAMESPACE 0x47435350 /* "GCSP" stands for "Ghc CallStack Profiler" */
#define CUSTOM_COMMAND_START_PROFILING 0x01
#define CUSTOM_COMMAND_STOP_PROFILING  0x02

void custom_ghc_stack_profiler_command_register(HsStablePtr);

#endif
