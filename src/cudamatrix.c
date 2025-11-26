#include <R.h>
#include <Rinternals.h>
#include <time.h>
#include <stdlib.h>

#define ROWS 1000000
#define COLS 1000
#define ITERATIONS 10

// Declare external functions from CUDA code
extern void matrixMultiply(int *A, int *B, int *C, int m, int n, int k);
extern void initializeMatrix(int *matrix, int rows, int cols);

// CUDA memory management functions
extern cudaError_t cudaMalloc(void** devPtr, size_t size);
extern cudaError_t cudaFree(void* devPtr);
extern cudaError_t cudaMemcpy(void* dst, const void* src, size_t count, cudaMemcpyKind kind);
extern cudaError_t cudaGetLastError(void);
extern const char* cudaGetErrorString(cudaError_t error);
extern cudaError_t cudaDeviceSynchronize(void);

// Main function that will be called from R
SEXP cuda_matrix_multiply(SEXP iterations) {
    // Get iterations from R
    int num_iterations = INTEGER(iterations)[0];
    if (num_iterations <= 0) {
        num_iterations = ITERATIONS;
    }
    
    // Set random seed
    srand(time(NULL));
    
    // Matrix dimensions
    int m = ROWS;    // 1,000,000 rows for A
    int k = COLS;    // 1,000 columns for A
    int n = COLS;    // Output will be 1,000 x 1,000
    
    // Allocate host memory
    size_t size_A = m * k * sizeof(int);
    size_t size_B = m * k * sizeof(int);
    size_t size_C = k * n * sizeof(int);
    
    int *h_A = (int*)malloc(size_A);
    int *h_B = (int*)malloc(size_B);
    int *h_C = (int*)malloc(size_C);
    
    if (h_A == NULL || h_B == NULL || h_C == NULL) {
        error("Host memory allocation failed!");
    }
    
    // Allocate device memory
    int *d_A, *d_B, *d_C;
    cudaError_t err = cudaMalloc((void**)&d_A, size_A);
    if (err != cudaSuccess) {
        free(h_A); free(h_B); free(h_C);
        error("CUDA memory allocation for d_A failed: %s", cudaGetErrorString(err));
    }
    
    err = cudaMalloc((void**)&d_B, size_B);
    if (err != cudaSuccess) {
        cudaFree(d_A);
        free(h_A); free(h_B); free(h_C);
        error("CUDA memory allocation for d_B failed: %s", cudaGetErrorString(err));
    }
    
    err = cudaMalloc((void**)&d_C, size_C);
    if (err != cudaSuccess) {
        cudaFree(d_A); cudaFree(d_B);
        free(h_A); free(h_B); free(h_C);
        error("CUDA memory allocation for d_C failed: %s", cudaGetErrorString(err));
    }
    
    // Set up CUDA grid and block dimensions
    dim3 blockDim(16, 16);
    dim3 gridDim((n + blockDim.x - 1) / blockDim.x,
                 (k + blockDim.y - 1) / blockDim.y);
    
    // Create result vector to return timing information
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    SEXP names = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(names, 0, mkChar("total_time_ms"));
    
    // Start timing
    clock_t start = clock();
    
    Rprintf("Matrix A: %d x %d\n", m, k);
    Rprintf("Matrix B: %d x %d\n", m, k);
    Rprintf("Output C: %d x %d\n", k, n);
    Rprintf("Starting %d iterations...\n", num_iterations);
    
    // Perform matrix multiplication for specified iterations
    for (int iter = 0; iter < num_iterations; iter++) {
        Rprintf("Iteration %d/%d\n", iter + 1, num_iterations);
        
        // Initialize matrices with random values
        initializeMatrix(h_A, m, k);
        initializeMatrix(h_B, m, k);
        
        // Copy matrices to device
        err = cudaMemcpy(d_A, h_A, size_A, cudaMemcpyHostToDevice);
        if (err != cudaSuccess) {
            cudaFree(d_A); cudaFree(d_B); cudaFree(d_C);
            free(h_A); free(h_B); free(h_C);
            error("CUDA memcpy (A) failed: %s", cudaGetErrorString(err));
        }
        
        err = cudaMemcpy(d_B, h_B, size_B, cudaMemcpyHostToDevice);
        if (err != cudaSuccess) {
            cudaFree(d_A); cudaFree(d_B); cudaFree(d_C);
            free(h_A); free(h_B); free(h_C);
            error("CUDA memcpy (B) failed: %s", cudaGetErrorString(err));
        }
        
        // Launch kernel for A^T * B to get C[1K x 1K]
        matrixMultiply<<<gridDim, blockDim>>>(d_B, d_A, d_C, k, n, m);
        
        // Check for kernel errors
        err = cudaGetLastError();
        if (err != cudaSuccess) {
            cudaFree(d_A); cudaFree(d_B); cudaFree(d_C);
            free(h_A); free(h_B); free(h_C);
            error("Kernel launch failed: %s", cudaGetErrorString(err));
        }
        
        // Wait for kernel to complete
        err = cudaDeviceSynchronize();
        if (err != cudaSuccess) {
            cudaFree(d_A); cudaFree(d_B); cudaFree(d_C);
            free(h_A); free(h_B); free(h_C);
            error("Device synchronization failed: %s", cudaGetErrorString(err));
        }
        
        // Copy result back to host
        err = cudaMemcpy(h_C, d_C, size_C, cudaMemcpyDeviceToHost);
        if (err != cudaSuccess) {
            cudaFree(d_A); cudaFree(d_B); cudaFree(d_C);
            free(h_A); free(h_B); free(h_C);
            error("CUDA memcpy (C) failed: %s", cudaGetErrorString(err));
        }
    }
    
    // Stop timing
    clock_t end = clock();
    double total_time = ((double)(end - start)) / CLOCKS_PER_SEC * 1000.0;
    
    REAL(result)[0] = total_time;
    setAttrib(result, R_NamesSymbol, names);
    
    // Cleanup
    cudaFree(d_A);
    cudaFree(d_B);
    cudaFree(d_C);
    free(h_A);
    free(h_B);
    free(h_C);
    
    UNPROTECT(2);
    return result;
}

// Initialization function for R
void R_init_cudamatrix(DllInfo *dll) {
    R_registerRoutines(dll, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}