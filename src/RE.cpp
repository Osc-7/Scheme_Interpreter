#include "RE.hpp"
#include <cstring>
#include <iostream>

RuntimeError ::RuntimeError(std ::string s1) : s(s1) {
  std::cout << s1 << std::endl;
}
std ::string RuntimeError ::message() const { return s; }