test_that("CudaMon GPU Matrix Multiplication benchmark workflow executes correctly", {
  library(CudaMon)
  library(bench)
  library(ggplot2)
  
  # Load the shared library (make sure it's compiled first)
  if (!file.exists("cudamatrix.o")) {
    stop("Please compile the CUDA code first using compile_cuda_code()")
  }
  dyn.load("cudamatrix.o")
  
  # R wrapper function
  cuda_matrix_multiply <- function(iterations = 10) {
    if (!is.loaded("cuda_matrix_multiply")) {
      stop("CUDA matrix multiply function not loaded. Please compile the C code first.")
    }
    
    result <- .Call("cuda_matrix_multiply", as.integer(iterations))
    return(result)
  }
  
  # Start collectl with GPU monitoring
  proc <- CudaMon::cl_start("cuda_matrix_multiply_benchmark", monitor_gpu = TRUE, gpu_monitor_type = "nvml")
  expect_true(proc$process$is_alive())
  CudaMon::cl_timestamp(proc, "start")
  
  cat("Starting CUDA Matrix Multiplication with GPU Monitoring...\n")
  cat("Matrix A: 1,000,000 x 1,000\n")
  cat("Matrix B: 1,000,000 x 1,000\n")
  cat("Output C: 1,000 x 1,000 (A^T * B)\n")
  cat("Iterations: 10\n")
  
  CudaMon::cl_timestamp(proc, "before_matrix_multiply")
  
  # Run the CUDA matrix multiplication with timing
  timing <- system.time({
    result <- cuda_matrix_multiply(iterations = 10)
  })
  
  CudaMon::cl_timestamp(proc, "after_matrix_multiply")
  
  cat("CUDA Matrix Multiplication Completed!\n")
  cat("Total time:", timing["elapsed"], "seconds\n")
  cat("R result (total time in ms):", result, "\n")
  
  # Benchmark with multiple iterations
  cat("\nRunning benchmark with GPU monitoring...\n")
  CudaMon::cl_timestamp(proc, "start_benchmark")
  
  bench_result <- bench::mark(
    cuda_matrix_multiply(iterations = 1),
    iterations = 5,
    check = FALSE
  )
  
  CudaMon::cl_timestamp(proc, "end_benchmark")
  
  print(bench_result)
  
  # Stop collectl and GPU monitoring
  CudaMon::cl_stop(proc)
  expect_false(proc$process$is_alive())
  
  # Validate result file and plot
  targ <- cl_result_path(proc)
  expect_true(file.exists(targ))
  parsed <- cl_parse(targ)
  expect_true(nrow(parsed) >= 5)
  
  # Print GPU usage statistics
  cat("\n=== GPU Usage Statistics ===\n")
  if ("gpu_usage" %in% names(parsed)) {
    gpu_stats <- summary(parsed$gpu_usage)
    cat("GPU Usage (%):\n")
    print(gpu_stats)
  }
  
  if ("gpu_mem_usage" %in% names(parsed)) {
    gpu_mem_stats <- summary(parsed$gpu_mem_usage)
    cat("\nGPU Memory Usage (%):\n")
    print(gpu_mem_stats)
  }
  
  # Create and display system metrics plot
  plot <- CudaMon::cl_plot_system_metrics(proc)
  expect_true("ggplot" %in% class(plot))
  print(plot)
  
  # Create detailed GPU usage plots
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # Plot GPU usage over time
    if ("gpu_usage" %in% names(parsed) && "timestamp" %in% names(parsed)) {
      gpu_usage_plot <- ggplot2::ggplot(parsed, ggplot2::aes(x = timestamp, y = gpu_usage)) +
        ggplot2::geom_line(color = "blue", linewidth = 1) +
        ggplot2::geom_point(color = "darkblue", size = 1) +
        ggplot2::labs(
          title = "GPU Usage During CUDA Matrix Multiplication",
          subtitle = "1M x 1K matrices multiplication (10 iterations)",
          x = "Time", 
          y = "GPU Usage (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      
      print(gpu_usage_plot)
    }
    
    # Plot GPU memory usage over time
    if ("gpu_mem_usage" %in% names(parsed) && "timestamp" %in% names(parsed)) {
      gpu_mem_plot <- ggplot2::ggplot(parsed, ggplot2::aes(x = timestamp, y = gpu_mem_usage)) +
        ggplot2::geom_line(color = "red", linewidth = 1) +
        ggplot2::geom_point(color = "darkred", size = 1) +
        ggplot2::labs(
          title = "GPU Memory Usage During CUDA Matrix Multiplication",
          subtitle = "1M x 1K matrices multiplication (10 iterations)",
          x = "Time", 
          y = "GPU Memory Usage (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      
      print(gpu_mem_plot)
    }
  }
  
  # Return comprehensive results
  test_results <- list(
    timing = timing,
    bench_result = bench_result,
    cuda_result = result,
    gpu_stats = if ("gpu_usage" %in% names(parsed)) summary(parsed$gpu_usage) else NULL,
    gpu_mem_stats = if ("gpu_mem_usage" %in% names(parsed)) summary(parsed$gpu_mem_usage) else NULL,
    monitoring_data = parsed
  )
  
  cat("\n=== Test Summary ===\n")
  cat("Matrix multiplication completed successfully\n")
  cat("GPU monitoring data collected:", nrow(parsed), "samples\n")
  cat("Average GPU usage:", if ("gpu_usage" %in% names(parsed)) mean(parsed$gpu_usage, na.rm = TRUE) else "N/A", "%\n")
  cat("Peak GPU memory usage:", if ("gpu_mem_usage" %in% names(parsed)) max(parsed$gpu_mem_usage, na.rm = TRUE) else "N/A", "%\n")
  
  return(invisible(test_results))
})

# Compile function (same as your original)
compile_cuda_code <- function() {
  cat("Compiling CUDA C code...\n")
  compile_cmd <- "nvcc -Xcompiler -fPIC -shared -o cudamatrix.o matrix_multiply.c -I/usr/share/R/include -L/usr/lib/R/lib -lR -lcudart"
  system(compile_cmd)
  
  if (file.exists("cudamatrix.o")) {
    cat("Compilation successful! Now you can run the test\n")
  } else {
    cat("Compilation failed. Please check your CUDA installation.\n")
  }
}

# Helper function to run the test
run_cuda_matrix_test <- function() {
  cat("Running CUDA Matrix Multiplication Test with GPU Monitoring...\n")
  cat("Make sure to compile the code first:\n")
  cat("> compile_cuda_code()\n")
  cat("Then run:\n")
  cat("> test_that('CudaMon GPU Matrix Multiplication benchmark workflow executes correctly', { ... })\n")
  
  # Check if compiled
  if (!file.exists("cudamatrix.o")) {
    cat("cudamatrix.o not found. Please compile first.\n")
    return(FALSE)
  }
  
  # Run the test
  test_results <- test_that("CudaMon GPU Matrix Multiplication benchmark workflow executes correctly", {
    # Test code from above
  })
  
  return(TRUE)
}