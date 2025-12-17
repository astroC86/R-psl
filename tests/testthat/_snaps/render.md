# render minimal kernel looks reasonable

    #include <Rcpp.h>
    #include <cstdint>
    
    // prototypes
    
    Rcpp::IntegerVector mul2(
        Rcpp::IntegerVector x
    );
    
    // definitions
    
    Rcpp::IntegerVector mul2(
        Rcpp::IntegerVector x
    ) {
        int32_t* ptr = reinterpret_cast<int32_t*>(x.begin());
        uint32_t n = x.size();
        for (uint32_t i = 0U; i < n; i += 1U) {
            ptr[i] = (ptr[i] * 2);
        }
        return x;
    }

