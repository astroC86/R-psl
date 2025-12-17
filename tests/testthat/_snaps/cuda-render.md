# render CUDA kernel and launcher looks reasonable

    Code
      code
    Output
      [1] "#include <cuda_runtime.h>\n#include <cstdint>\n\n// prototypes\n\n__host__\nvoid mul2(\n    uint32_t n,\n    int32_t* x\n);\n\n__global__\nvoid mul2_kernel(\n    uint32_t n,\n    int32_t* x\n);\n\n// definitions\n\n__host__\nvoid mul2(\n    uint32_t n,\n    int32_t* x\n) {\n    uint32_t grid = ((n + 255U) / 256U);\n    uint32_t block = 256U;\n    mul2_kernel<<<grid, block>>>(n, x);\n}\n\n__global__\nvoid mul2_kernel(\n    uint32_t n,\n    int32_t* x\n) {\n    uint32_t i = ((blockIdx.x * blockDim.x) + threadIdx.x);\n    if ((i < n)) {\n        x[i] = (x[i] * 2);\n    }\n}\n"

# render CUDA kernel auto wrapper looks reasonable

    Code
      code
    Output
      [1] "#include <Rcpp.h>\n#include <cuda_runtime.h>\n#include <cstdint>\n\n// prototypes\n\n__host__\nRcpp::IntegerVector mul2_cuda(\n    Rcpp::IntegerVector x\n);\n\n__global__\nvoid mul2_kernel(\n    uint32_t n,\n    int32_t* x\n);\n\n// definitions\n\n__host__\nRcpp::IntegerVector mul2_cuda(\n    Rcpp::IntegerVector x\n) {\n    uint32_t n = x.size();\n    uint32_t grid = ((n + 255U) / 256U);\n    uint32_t block = 256U;\n    {\n        cudaError_t td_err;\n        int32_t* td_h_x = reinterpret_cast<int32_t*>(x.begin());\n        int32_t* td_d_x = nullptr;\n        size_t td_bytes_x = (size_t)x.size() * sizeof(int32_t);\n        td_err = cudaMalloc((void**)&td_d_x, td_bytes_x);\n        if (td_err != cudaSuccess) Rcpp::stop(cudaGetErrorString(td_err));\n        td_err = cudaMemcpy(td_d_x, td_h_x, td_bytes_x, cudaMemcpyHostToDevice);\n        if (td_err != cudaSuccess) Rcpp::stop(cudaGetErrorString(td_err));\n        \n        mul2_kernel<<<grid, block>>>(n, td_d_x);\n        td_err = cudaDeviceSynchronize();\n        if (td_err != cudaSuccess) Rcpp::stop(cudaGetErrorString(td_err));\n        td_err = cudaMemcpy(td_h_x, td_d_x, td_bytes_x, cudaMemcpyDeviceToHost);\n        if (td_err != cudaSuccess) Rcpp::stop(cudaGetErrorString(td_err));\n        cudaFree(td_d_x);\n        \n    }\n    return x;\n}\n\n__global__\nvoid mul2_kernel(\n    uint32_t n,\n    int32_t* x\n) {\n    uint32_t i = ((blockIdx.x * blockDim.x) + threadIdx.x);\n    if ((i < n)) {\n        x[i] = (x[i] * 2);\n    }\n}\n"

