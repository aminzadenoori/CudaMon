test_that("CudaMon GPU PCA benchmark workflow executes correctly", {
  library(CudaMon)
  library(reticulate)
  library(Matrix)
  library(bench)
  library(bgmeter)
  library(ggplot2)
  
  # Initialize Python and import modules
  use_python("/usr/bin/python3") # Adjust path if needed
  np <- import("numpy")
  cp <- import("cupy")
  
  # Start collectl with GPU monitoring
  proc <- CudaMon::cl_start("temp", monitor_gpu = TRUE, gpu_monitor_type = "nvml")
  expect_true(proc$process$is_alive())
  CudaMon::cl_timestamp(proc, "start")
  
  # Create dense matrix
  create_dense_matrix <- function(n_rows = 1e5, n_cols = 1e3) {
    matrix_data <- sample(-128L:127L, n_rows * n_cols, replace = TRUE)
    matrix(matrix_data, nrow = as.integer(n_rows), ncol = as.integer(n_cols), byrow = TRUE)
  }
  dense_m <- create_dense_matrix()
  expect_true(is.matrix(dense_m))
  expect_equal(dim(dense_m), c(1e5, 1e3))
  CudaMon::cl_timestamp(proc, "create_matrix")
  
  # Convert to GPU array
  to_gpu <- function(dense_m) {
    cp$array(dense_m, dtype = cp$int8)
  }
  gpu_dense <- to_gpu(dense_m)
  expect_true("cupy._core.core.ndarray" %in% class(gpu_dense))
  CudaMon::cl_timestamp(proc, "to_gpu")
  
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
  CudaMon::cl_timestamp(proc, "run_pca")
  
  # Benchmark PCA
  benchmark_pca <- function() {
    bench::mark(
      pca = run_gpu_pca(gpu_dense),
      iterations = 5L,
      check = FALSE
    )
  }
  results <- benchmark_pca()
  expect_true("bench_mark" %in% class(results))
  expect_true(nrow(results) >= 1)
  CudaMon::cl_timestamp(proc, "end")
  
  # Stop collectl and GPU monitoring
  CudaMon::cl_stop(proc)
  expect_false(proc$process$is_alive())
  
  # Validate result file and plot
  targ <- cl_result_path(proc)
  expect_true(file.exists(targ))
  parsed <- cl_parse(targ)
  expect_true(nrow(parsed) >= 5)
  
  plot <- CudaMon::cl_plot_system_metrics(proc)
  expect_true("ggplot" %in% class(plot))
})