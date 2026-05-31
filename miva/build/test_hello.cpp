#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include <mvp_builtin.h>


using namespace std;

namespace mvp_main {

mvp_builtin_unit mvp_own_main(mvp_builtin_int argc) {
  mvp_println(mvp_builtin_string("Hello, World"));
  return mvp_builtin_void;
}

}

int main(int argc, char** argv)
{
  mvp_own_main(argc);
  return 0;
}


