#include <cuda_runtime.h>
#include <R.h>

#define ROWS 1000000
#define COLS 1000

// CUDA kernel for matrix multiplication: A[m x k] * B[k x n] = C[m x n]
extern "C" __global__ void matrixMultiply(int *A, int *B, int *C, int m, int n, int k) {
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;
    
    if (row < m && col < n) {
        int sum = 0;
        for (int i = 0; i < k; i++) {
            sum += A[row * k + i] * B[col * k + i]; // B[col * k + i] accesses B^T
        }
        C[row * n + col] = sum;
    }
}

// Alternative kernel if we want A * B without transpose
extern "C" __global__ void matrixMultiplyLarge(int *A, int *B, int *C, int m, int n, int k) {
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;
    
    if (row < m && col < n) {
        int sum = 0;
        for (int i = 0; i < k; i++) {
            sum += A[row * k + i] * B[i * n + col];
        }
        C[row * n + col] = sum;
    }
}

// Function to initialize matrix with random values
extern "C" void initializeMatrix(int *matrix, int rows, int cols) {
    for (int i = 0; i < rows * cols; i++) {
        matrix[i] = (rand() % 256) - 128; // Values between -128 and 127
    }
}