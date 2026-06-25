#ifndef MVP_TEST_H
#define MVP_TEST_H

#include <chrono>
#include <cinttypes>
#include <cstdint>
#include <cstdio>
#include <stdexcept>

#include "mvp_builtin.h"

#define MVP_COLOR_RED "\x1b[0;31m"
#define MVP_COLOR_GREEN "\x1b[0;32m"
#define MVP_COLOR_YELLOW "\x1b[0;33m"
#define MVP_COLOR_CYAN "\x1b[0;36m"
#define MVP_COLOR_RESET "\x1b[0m"

using mvp_test_fn = mvp_builtin_int (*)();

struct MvpTest {
  const char *name;
  mvp_test_fn fn;
};

static int mvp_run_tests(MvpTest tests[], int count) {
  int passed = 0;
  int failed = 0;

  for (int i = 0; i < count; i++) {
    auto start = std::chrono::steady_clock::now();
    try {
      mvp_builtin_int res = tests[i].fn();
      auto end = std::chrono::steady_clock::now();
      auto ms =
          std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
              .count();
      if (res == 0) {
        std::printf("%sPASS%s %s (%ldms)\n", MVP_COLOR_GREEN, MVP_COLOR_RESET,
                    tests[i].name, ms);
        passed++;
      } else {
        std::printf("%sFAIL%s %s: returned %" PRId64 " (%ldms)\n",
                    MVP_COLOR_RED, MVP_COLOR_RESET, tests[i].name, res, ms);
        failed++;
      }
    } catch (const std::exception &e) {
      auto end = std::chrono::steady_clock::now();
      auto ms =
          std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
              .count();
      std::printf("%sFAIL%s %s: %s (%ldms)\n", MVP_COLOR_RED, MVP_COLOR_RESET,
                  tests[i].name, e.what(), ms);
      failed++;
    }
  }

  if (failed == 0) {
    std::printf("%s0/%d tests failed.%s\n", MVP_COLOR_CYAN, count,
                MVP_COLOR_RESET);
    std::printf("------------------------------\n");
    return 0;
  } else {
    std::printf("%s%d/%d tests failed.%s\n", MVP_COLOR_CYAN, failed, count,
                MVP_COLOR_RESET);
    std::printf("------------------------------\n");
    return 1;
  }
}

#endif
