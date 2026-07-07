#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <mvp_builtin.h>


using namespace std;

using namespace mvp_std::str;
namespace mvp_main {

 mvp_builtin_int add(mvp_builtin_int a, mvp_builtin_int b) { return (a + b); }

 mvp_builtin_int mul(mvp_builtin_int a, mvp_builtin_int b) { return (a * b); }

mvp_builtin_unit mvp_own_main(mvp_builtin_int argc) {
  ([&]() {
    auto s = mvp_builtin_string("");
    s = ((std::move(s) + mvp_to_string(add(static_cast<mvp_builtin_int>(3), static_cast<mvp_builtin_int>(4)))) + mvp_builtin_string(" "));
    s = ((std::move(s) + mvp_to_string(mul(static_cast<mvp_builtin_int>(5), static_cast<mvp_builtin_int>(6)))) + mvp_builtin_string(" "));
    s = ((std::move(s) + mvp_to_string(add(mul(static_cast<mvp_builtin_int>(2), static_cast<mvp_builtin_int>(3)), static_cast<mvp_builtin_int>(1)))) + mvp_builtin_string(" "));
    mvp_print(s);
    mvp_print(mvp_builtin_string("\n"));
  })();
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


