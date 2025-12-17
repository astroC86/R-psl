# render CUDA kernel and launcher looks reasonable

    Code
      code
    Output
      [1] "#include <cuda_runtime.h>\n#include <cstdint>\n\n// prototypes\n\n__host__\nvoid mul2(\n    uint32_t n,\n    int32_t* x\n);\n\n__global__\nvoid mul2_kernel(\n    uint32_t n,\n    int32_t* x\n);\n\n// definitions\n\n__host__\nvoid mul2(\n    uint32_t n,\n    int32_t* x\n) {\n    uint32_t grid = ((n + 255U) / 256U);\n    uint32_t block = 256U;\n    mul2_kernel<<<grid, block>>>(n, x);\n}\n\n__global__\nvoid mul2_kernel(\n    uint32_t n,\n    int32_t* x\n) {\n    uint32_t i = ((blockIdx.x * blockDim.x) + threadIdx.x);\n    if ((i < n)) {\n        x[i] = (x[i] * 2);\n    }\n}\n"

