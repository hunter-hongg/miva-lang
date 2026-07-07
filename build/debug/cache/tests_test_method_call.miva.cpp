#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <mvp_builtin.h>


using namespace std;

namespace mvp_main {

 mvp_builtin_int twice(mvp_builtin_int x) { return (x * static_cast<mvp_builtin_int>(2)); }

 mvp_builtin_int add(mvp_builtin_int a, mvp_builtin_int b) { return (a + b); }

 mvp_builtin_int triple(mvp_builtin_int x) { return (x * static_cast<mvp_builtin_int>(3)); }

mvp_builtin_unit mvp_own_main(mvp_builtin_int argc) {
  const auto n = static_cast<mvp_builtin_int>(10);
  mvp_prints(twice(n));
  mvp_prints(mvp_builtin_string(" "));
  mvp_prints(add(n, static_cast<mvp_builtin_int>(5)));
  mvp_prints(mvp_builtin_string(" "));
  mvp_prints(add(twice(n), static_cast<mvp_builtin_int>(5)));
  mvp_prints(mvp_builtin_string(" "));
  mvp_prints(add(add(n, static_cast<mvp_builtin_int>(3)), add(n, static_cast<mvp_builtin_int>(4))));
  mvp_prints(mvp_builtin_string(" "));
  mvp_prints(triple(add(n, static_cast<mvp_builtin_int>(2))));
  mvp_print(mvp_builtin_string("\n"));
  return mvp_builtin_void;
}

}

int main(int argc, char** argv)
{
  try {
  mvp_main::mvp_own_main(argc);
  } catch (std::exception& e) {
     mvp_errorlns("panic: ", e.what());}
  return 0;
}


