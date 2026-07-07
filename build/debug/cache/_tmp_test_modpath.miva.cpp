#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <mvp_builtin.h>


using namespace std;

using namespace mvp_std::str;
namespace mvp_main {

mvp_builtin_unit mvp_own_main(mvp_builtin_int argc) {
  const auto q = mvp_std::str::from(static_cast<mvp_builtin_int>(1));
  mvp_prints(q);
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


