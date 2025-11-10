library(CudaMon)


# Initialize Python
use_python("/usr/bin/python3") # Adjust path if needed
np <- import("numpy")
cp <- import("cupy")

# Start collectl with GPU monitoring
proc <- CudaMon::cl_start("temp", monitor_gpu = TRUE, gpu_monitor_type = "nvml")

# Add timestamps for workflow steps
CudaMon::cl_timestamp(proc, "start")

# Create dense matrix
create_dense_matrix <- function(n_rows = 1e5, n_cols = 1e3) {
  matrix_data <- sample(-128L:127L, n_rows * n_cols, replace = TRUE)
  matrix(matrix_data, nrow = as.integer(n_rows), ncol = as.integer(n_cols), byrow = TRUE)
}

# Convert to GPU array
to_gpu <- function(dense_m) {
  cp$array(dense_m, dtype = cp$int8)
}

# Run PCA on GPU
run_gpu_pca <- function(gpu_dense, n_components = 50L) {
  data_f32 <- gpu_dense$astype(cp$float32)
  mean <- cp$mean(data_f32, axis = 0L)
  centered <- data_f32 - mean
  svd_result <- cp$linalg$svd(centered, full_matrices = FALSE)
  vh <- svd_result[[3]]
  components <- vh$T[, 0L:(as.integer(n_components)-1L)]
  return(components)
}

# Benchmark PCA
benchmark_pca <- function() {
  cat("Creating dense matrix...\n")
  CudaMon::cl_timestamp(proc, "create_matrix")
  dense_m <- create_dense_matrix()
  
  cat("Transferring to GPU...\n")
  CudaMon::cl_timestamp(proc, "to_gpu")
  gpu_dense <- to_gpu(dense_m)
  
  cat("Running PCA...\n")
  CudaMon::cl_timestamp(proc, "run_pca")
  bench_result <- bench::mark(
    pca = run_gpu_pca(gpu_dense),
    iterations = 5L,
    check = FALSE
  )
  
  return(bench_result)
}

# Run benchmark
results <- benchmark_pca()
CudaMon::cl_timestamp(proc, "end")

# Stop collectl and GPU monitoring
CudaMon::cl_stop(proc)

CudaMon::cl_plot_system_metrics(proc)