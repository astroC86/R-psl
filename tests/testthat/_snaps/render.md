# render minimal kernel looks reasonable

    Code
      code
    Output
      [1] "#include <Rcpp.h>\n#include <cstdint>\n\n// prototypes\n\nRcpp::IntegerVector mul2(\n    Rcpp::IntegerVector x\n);\n\n// definitions\n\nRcpp::IntegerVector mul2(\n    Rcpp::IntegerVector x\n) {\n    int32_t* ptr = reinterpret_cast<int32_t*>(x.begin());\n    uint32_t n = x.size();\n    for (uint32_t i = 0U; i < n; i += 1U) {\n        ptr[i] = (ptr[i] * 2);\n    }\n    return x;\n}\n"

