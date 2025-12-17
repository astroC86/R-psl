# render CUDA kernel and launcher looks reasonable

    #include <cuda_runtime.h>
    #include <cstdint>
    
    // prototypes
    
    __host__
    void mul2(
        uint32_t n,
        int32_t* x
    );
    
    __global__
    void mul2_kernel(
        uint32_t n,
        int32_t* x
    );
    
    // definitions
    
    __host__
    void mul2(
        uint32_t n,
        int32_t* x
    ) {
        uint32_t grid = ((n + 255U) / 256U);
        uint32_t block = 256U;
        mul2_kernel<<<grid, block>>>(n, x);
    }
    
    __global__
    void mul2_kernel(
        uint32_t n,
        int32_t* x
    ) {
        uint32_t i = ((blockIdx.x * blockDim.x) + threadIdx.x);
        if ((i < n)) {
            x[i] = (x[i] * 2);
        }
    }

