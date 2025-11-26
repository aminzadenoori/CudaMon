# Load the shared library
#dyn.load("cudamatrix.o")

# R wrapper function
cuda_matrix_multiply <- function(iterations = 10) {
  if (!is.loaded("cuda_matrix_multiply")) {
    stop("CUDA matrix multiply function not loaded. Please compile the C code first.")
  }
  
  result <- .Call("cuda_matrix_multiply", as.integer(iterations))
  return(result)
}

# Test function
test_cuda_matrix_multiply <- function() {
  library(bench)
  
  cat("Starting CUDA Matrix Multiplication Test...\n")
  cat("Matrix A: 1,000,000 x 1,000\n")
  cat("Matrix B: 1,000,000 x 1,000\n")
  cat("Output C: 1,000 x 1,000 (A^T * B)\n")
  cat("Iterations: 10\n")
  
  # Time the operation
  timing <- system.time({
    result <- cuda_matrix_multiply(iterations = 10)
  })
  
  cat("CUDA Matrix Multiplication Completed!\n")
  cat("Total time:", timing["elapsed"], "seconds\n")
  cat("R result:", result, "\n")
  
  # Benchmark with multiple iterations
  cat("\nRunning benchmark...\n")
  bench_result <- bench::mark(
    cuda_matrix_multiply(iterations = 1),
    iterations = 5,
    check = FALSE
  )
  
  print(bench_result)
  
  return(list(
    timing = timing,
    bench_result = bench_result,
    cuda_result = result
  ))
}

# Compile function
compile_cuda_code <- function() {
  cat("Compiling CUDA C code...\n")
  compile_cmd <- "nvcc -Xcompiler -fPIC -shared -o cudamatrix.o matrix_multiply.c -I/usr/share/R/include -L/usr/lib/R/lib -lR -lcudart"
  system(compile_cmd)
  
  if (file.exists("cudamatrix.o")) {
    cat("Compilation successful! Now you can run test_cuda_matrix_multiply()\n")
  } else {
    cat("Compilation failed. Please check your CUDA installation.\n")
  }
}