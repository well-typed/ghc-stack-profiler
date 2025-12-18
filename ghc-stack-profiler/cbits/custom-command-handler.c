#include <stdbool.h>
#include <stdio.h>

#include <Rts.h>
#include <eventlog_socket.h>

#include "custom-command-handler.h"

extern void startProfiler(HsStablePtr ptr);
extern void stopProfiler(HsStablePtr ptr);

static void ghc_stack_profiler_start_handler(uint32_t namespace_id, uint8_t cmd_id, void *user_data)
{
  const HsStablePtr stack_profiler = (HsStablePtr) user_data;
  fprintf(stderr,
          "[custom-command] received namespace=0x%08x id=0x%02x (start profiling)\n",
          namespace_id,
          cmd_id);
  startProfiler(stack_profiler);
}

static void ghc_stack_profiler_stop_handler(uint32_t namespace_id, uint8_t cmd_id, void *user_data)
{
  const HsStablePtr stack_profiler = (HsStablePtr) user_data;
  fprintf(stderr,
          "[custom-command] received namespace=0x%08x id=0x%02x (stop profiling)\n",
          namespace_id,
          cmd_id);
  stopProfiler(stack_profiler);
}


static void ghc_stack_profiler_control_command_register(uint8_t cmd_id, eventlog_control_command_handler handler, HsStablePtr stack_profiler_ptr)
{
  bool ok = eventlog_socket_register_control_command(
      CUSTOM_COMMAND_NAMESPACE,
      cmd_id,
      handler,
      (void *)stack_profiler_ptr);
  if (!ok) {
      fprintf(stderr,
              "[custom-command] failed to register namespace=0x%08x id=0x%02x\n",
              CUSTOM_COMMAND_NAMESPACE,
              cmd_id);
  }
}

void custom_ghc_stack_profiler_command_register(HsStablePtr stack_profiler_ptr)
{
  ghc_stack_profiler_control_command_register(CUSTOM_COMMAND_START_PROFILING, ghc_stack_profiler_start_handler, stack_profiler_ptr);
  ghc_stack_profiler_control_command_register(CUSTOM_COMMAND_STOP_PROFILING, ghc_stack_profiler_stop_handler, stack_profiler_ptr);
}
