#include <stdbool.h>
#include <stdio.h>

#include <Rts.h>
#include <eventlog_socket.h>
#include <string.h>

#include "custom-command-handler.h"

extern void startProfiler(HsStablePtr ptr);
extern void stopProfiler(HsStablePtr ptr);

// Debug macros
#define DEBUG_INFO(fmt, ...)                                                   \
  do {                                                                         \
    fprintf(stderr, "INFO[%s|%d|%s]: " fmt "\n", __FILE__, __LINE__, __func__, \
            __VA_ARGS__);                                                      \
  } while (0)

/// @brief If the status contains an error code, print a string describing the
/// error to stderr, and exit.
#define EXIT_ON_ERROR(eventlog_socket_status)                                  \
  do {                                                                         \
    const EventlogSocketStatus status = (eventlog_socket_status);              \
    const EventlogSocketStatusCode status_code = status.ess_status_code;       \
    if (status_code != EVENTLOG_SOCKET_OK) {                                   \
      char *strerr = eventlog_socket_strerror(status);                         \
      if (strerr != NULL) {                                                    \
        fprintf(stderr, "ERROR[%s|%d|%s]: %s\n", __FILE__, __LINE__, __func__, \
                strerr);                                                       \
        free(strerr);                                                          \
        exit((int)status_code); /* NOLINT */                                   \
      }                                                                        \
    }                                                                          \
  } while (0)

static void ghc_stack_profiler_start_handler(
    const EventlogSocketControlNamespace *const namespace,
    const EventlogSocketControlCommandId command_id, const void *user_data) {
  const HsStablePtr stack_profiler = (HsStablePtr)user_data;
  DEBUG_INFO("[custom-command] received namespace=0x%08x id=0x%02x (start "
             "profiling)\n",
             namespace, command_id);
  startProfiler(stack_profiler);
}

static void ghc_stack_profiler_stop_handler(
    const EventlogSocketControlNamespace *const namespace,
    const EventlogSocketControlCommandId command_id, const void *user_data) {
  const HsStablePtr stack_profiler = (HsStablePtr)user_data;
  DEBUG_INFO(
      "[custom-command] received namespace=0x%08x id=0x%02x (stop profiling)\n",
      namespace, command_id);
  stopProfiler(stack_profiler);
}

static void ghc_stack_profiler_control_command_register(
    EventlogSocketControlNamespace *namespace, uint8_t cmd_id,
    EventlogSocketControlCommandHandler handler,
    HsStablePtr stack_profiler_ptr) {
  EXIT_ON_ERROR(eventlog_socket_control_register_command(
      namespace, cmd_id, handler, stack_profiler_ptr));
}

void custom_ghc_stack_profiler_command_register(
    HsStablePtr stack_profiler_ptr) {
  // Register the "custom-command" namespace.
  EventlogSocketControlNamespace *namespace = NULL;
  EXIT_ON_ERROR(eventlog_socket_control_register_namespace(
      strlen(CUSTOM_COMMAND_NAMESPACE), CUSTOM_COMMAND_NAMESPACE, &namespace));

  ghc_stack_profiler_control_command_register(
      namespace, CUSTOM_COMMAND_START_PROFILING,
      ghc_stack_profiler_start_handler, stack_profiler_ptr);
  ghc_stack_profiler_control_command_register(
      namespace, CUSTOM_COMMAND_STOP_PROFILING, ghc_stack_profiler_stop_handler,
      stack_profiler_ptr);
}
