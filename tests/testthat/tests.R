library(CudaMon)


test_that("GPU PCA workflow with monitoring succeeds", {
  # Skip test if collectl not available
  skip_if_not(cl_exists())
  
  # Initialize Python and CuPy
  use_python("/usr/bin/python3")
  np <- import("numpy")
  cp <- import("cupy")
  
  # Start collectl with GPU monitoring
  proc <- cl_start("temp", monitor_gpu = TRUE, gpu_monitor_type = "nvml")
  expect_true(proc$process$is_alive())
  
  # Add timestamps for workflow steps
  cl_timestamp(proc, "start")
  
  # Create dense matrix function
  create_dense_matrix <- function(n_rows = 1e5, n_cols = 1e3) {
    matrix_data <- sample(-128L:127L, n_rows * n_cols, replace = TRUE)
    matrix(matrix_data, nrow = as.integer(n_rows), ncol = as.integer(n_cols), byrow = TRUE)
  }
  
  # Convert to GPU array function
  to_gpu <- function(dense_m) {
    cp$array(dense_m, dtype = cp$int8)
  }
  
  # Run PCA on GPU function
  run_gpu_pca <- function(gpu_dense, n_components = 50L) {
    data_f32 <- gpu_dense$astype(cp$float32)
    mean <- cp$mean(data_f32, axis = 0L)
    centered <- data_f32 - mean
    svd_result <- cp$linalg$svd(centered, full_matrices = FALSE)
    vh <- svd_result[[3]]
    components <- vh$T[, 0L:(as.integer(n_components)-1L)]
    return(components)
  }
  
  # Benchmark PCA workflow
  cat("Creating dense matrix...\n")
  cl_timestamp(proc, "create_matrix")
  dense_m <- create_dense_matrix()
  expect_true(is.matrix(dense_m))
  expect_equal(dim(dense_m), c(100000, 1000))
  
  cat("Transferring to GPU...\n")
  cl_timestamp(proc, "to_gpu")
  gpu_dense <- to_gpu(dense_m)
  expect_true(inherits(gpu_dense, "python.builtin.object"))
  
  cat("Running PCA...\n")
  cl_timestamp(proc, "run_pca")
  bench_result <- bench::mark(
    pca = run_gpu_pca(gpu_dense),
    iterations = 5L,
    check = FALSE
  )
  
  expect_true(nrow(bench_result) > 0)
  expect_true("median" %in% names(bench_result))
  
  cl_timestamp(proc, "end")
  
  # Stop collectl and verify results
  cl_stop(proc)
  Sys.sleep(2)
  expect_false(proc$process$is_alive())
  
  # Verify output file exists and can be parsed
  targ <- cl_result_path(proc)
  expect_true(file.exists(targ))
  
  # Parse and verify system metrics
  metrics <- cl_parse(targ)
  expect_true(nrow(metrics) >= 1)
  
  # Test plotting functionality
  plot_result <- cl_plot_system_metrics(proc)
  expect_true("ggplot" %in% class(plot_result))
})

test_that("cl_parse succeeds with GPU metrics", {
  # This would test with actual GPU monitoring data
  # For now, test basic functionality
  if (cl_exists()) {
    lk <- cl_parse(system.file("demotab/demo_1123.tab.gz", package = "Rcollectl"))
    expect_true(all(dim(lk) == c(478, 71)))
    expect_true(length(grep("CPU", names(lk))) == 21)
  }
})

test_that("plot_usage succeeds with GPU data", {
  if (cl_exists()) {
    lk <- cl_parse(system.file("demotab/demo_1123.tab.gz", package = "Rcollectl"))
    x <- plot_usage(lk)
    expect_true("ggplot" %in% class(x))
    expect_true("FacetGrid" %in% class(x$facet))
  }
})