#' Check for collectl availability
#' @return logical(1)
#' @examples
#' cl_exists()
#' @export
cl_exists <- function() {
  Rcollectl::cl_exists()
}

#' Start collectl with optional GPU monitoring
#' @importFrom Rcollectl cl_start cl_stop cl_result_path
#' @importFrom processx process
#' @importFrom bgmeter bgmeter_start bgmeter_stop
#' @param target character(1) path; destination of collectl report
#' @param monitor_gpu logical(1) whether to enable GPU monitoring
#' @param gpu_monitor_type either "collectl" or "nvml" for GPU monitoring
#' @return instance of `Rcollectl_process` with monitoring components
#' @export
cl_start <- function(target = tempfile(), 
                     monitor_gpu = FALSE,
                     gpu_monitor_type = c("collectl", "nvml")) {
  gpu_monitor_type <- match.arg(gpu_monitor_type)
  
  # Start base collectl process using Rcollectl package
  base_proc <- Rcollectl::cl_start(target = target)
  
  # Initialize GPU monitoring components
  gpu_monitor <- NULL
  bgmeter_process <- NULL
  gpu_metrics_file <- NULL
  
  if (monitor_gpu) {
    if (gpu_monitor_type == "collectl" && cl_gpu_exists()) {
      # For collectl GPU monitoring, we need to stop and restart with GPU args
      Rcollectl::cl_stop(base_proc)
      args <- c("-scdmnG", "-P", paste("-f", target, sep=""))
      proc <- try(processx::process$new("collectl", args = args))
      base_proc$process <- proc
    } else if (gpu_monitor_type == "nvml" && nvml_available()) {
      gpu_monitor <- GPUMonitor$new()
      if (requireNamespace("bgmeter", quietly = TRUE)) {
        gpu_metrics_file <- tempfile("gpumetrics", fileext = ".jsonl")
        
        # Define function to collect GPU metrics
        measure_gpu <- function() {
          metrics <- gpu_monitor$get_metrics()
          lapply(metrics, function(m) {
            list(
              timestamp = as.character(Sys.time()),
              metrics = m
            )
          })
        }
        
        # Start background GPU metrics collection every second
        bgmeter_process <- bgmeter_start(measure_gpu, 1L, gpu_metrics_file, "log.txt")
      } else {
        warning("bgmeter package not installed. GPU metrics will not be collected in the background.")
      }
    } else {
      warning("Requested GPU monitoring not available")
    }
  }
  
  # Enhance the base Rcollectl_process with GPU monitoring components
  base_proc$gpu_monitor <- gpu_monitor
  base_proc$gpu_monitor_type <- if (monitor_gpu) gpu_monitor_type else NULL
  base_proc$bgmeter_process <- bgmeter_process
  base_proc$gpu_metrics_file <- gpu_metrics_file
  
  class(base_proc) <- "Rcollectl_process"
  base_proc
}

#' Get GPU metrics from Rcollectl_process
#' @param proc Rcollectl_process object
#' @return list of GPU metrics or NULL if not monitoring
#' @export
cl_get_gpu_metrics <- function(proc) {
  if (!inherits(proc, "Rcollectl_process")) {
    stop("Not an Rcollectl_process object")
  }
  
  if (!is.null(proc$gpu_monitor)) {
    if (proc$gpu_monitor_type == "nvml") {
      return(proc$gpu_monitor$get_all_metrics())
    }
  }
  return(NULL)
}

#' Print method for Rcollectl process
#' @param x an entity inheriting from "Rcollectl_process" S3 class
#' @param \dots not used
#' @return invisibly returns the input
#' @examples
#' example(cl_start)
#' @export
print.Rcollectl_process <- function(x, ...) {
  # Use Rcollectl's print method for the base components
  Rcollectl::print.Rcollectl_process(x)
  
  # Add GPU-specific information
  if (!is.null(x$gpu_metrics_file)) {
    cat("  GPU metrics file: ", x$gpu_metrics_file, "\n")
  }
  if (!is.null(x$gpu_monitor_type)) {
    cat("  GPU monitor type: ", x$gpu_monitor_type, "\n")
  }
  invisible(x)
}

#' Stop collectl via processx interrupt
#' @param proc an entity inheriting from "Rcollectl_process" S3 class
#' @return invisibly returns the input
#' @examples
#' example(cl_start)
#' @export
cl_stop <- function(proc) {
  stopifnot(inherits(proc, "Rcollectl_process"))
  
  # Stop base collectl process using Rcollectl package
  Rcollectl::cl_stop(proc)
  
  # Stop GPU monitoring components
  if (!is.null(proc$bgmeter_process)) {
    bgmeter_stop(proc$bgmeter_process)
  }
  if (!is.null(proc$gpu_monitor)) {
    proc$gpu_monitor$finalize()
  }
  invisible(proc)
}

#' Get full path to collectl report
#' @param proc an entity inheriting from "Rcollectl_process" S3 class
#' @return character(1) path to report
#' @examples
#' example(cl_start)
#' @export
cl_result_path <- function(proc) {
  Rcollectl::cl_result_path(proc)
}

#' Get path to GPU metrics file
#' @param proc an entity inheriting from "Rcollectl_process" S3 class
#' @return character(1) path to GPU metrics file or NULL if not available
#' @export
cl_gpu_metrics_path <- function(proc) {
  stopifnot(inherits(proc, "Rcollectl_process"))
  proc$gpu_metrics_file
}

#' Plot system metrics including CPU and GPU usage
#' @param proc Rcollectl_process object
#' @importFrom jsonlite fromJSON
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal theme element_text
#' @importFrom gridExtra grid.arrange
#' @export
cl_plot_system_metrics <- function(proc) {
  # Read CPU data using Rcollectl result path
  cpu_file <- Rcollectl::cl_result_path(proc)
  cpu_data <- read.table(cpu_file, skip = 9, header = FALSE, comment.char = "#")
  
  # Create simple integer sequence for x-axis (1 to number of observations)
  x_values <- 1:nrow(cpu_data)
  
  # Extract CPU usage and memory usage
  cpu_metrics <- data.frame(
    x = x_values,
    cpu_usage = cpu_data$V10,
    memory_used_mb = cpu_data$V23 / 1024,  # Convert kB to MB
    memory_total_mb = cpu_data$V24 / 1024   # Convert kB to MB
  )
  
  # Initialize GPU metrics as NULL
  gpu_metrics <- NULL
  
  # Read GPU data if available
  if (!is.null(proc$gpu_metrics_file) && file.exists(proc$gpu_metrics_file)) {
    gpu_lines <- readLines(proc$gpu_metrics_file)
    gpu_data <- lapply(gpu_lines, jsonlite::fromJSON)
    
    # Extract GPU metrics and use same x-values (assuming same number of observations)
    gpu_x_values <- 1:length(gpu_data)
    
    gpu_metrics <- data.frame(
      x = gpu_x_values,
      gpu_usage = sapply(gpu_data, function(x) x$gpu_util$`2`$gpu_util),
      gpu_memory_used_mb = sapply(gpu_data, function(x) x$gpu_util$`2`$memory_used),
      gpu_memory_total_mb = sapply(gpu_data, function(x) x$gpu_util$`2`$memory_total)
    )
  }
  
  # Create plots
  plots <- list()
  
  # CPU usage plot
  plots[[1]] <- ggplot2::ggplot(cpu_metrics, ggplot2::aes(x = x, y = cpu_usage)) +
    ggplot2::geom_point(size = 1) +
    ggplot2::labs(title = "CPU", x = "Time (seconds)", y = "Usage (%)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
  
  # CPU memory plot
  plots[[2]] <- ggplot2::ggplot(cpu_metrics, ggplot2::aes(x = x, y = memory_used_mb)) +
    ggplot2::geom_point(size = 1) +
    ggplot2::labs(title = "CPUM", x = "Time (seconds)", y = "Memory (MB)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
  
  # GPU plots if available
  if (!is.null(gpu_metrics)) {
    plots[[3]] <- ggplot2::ggplot(gpu_metrics, ggplot2::aes(x = x, y = gpu_usage)) +
      ggplot2::geom_point(size = 1) +
      ggplot2::labs(title = "GPU", x = "Time (seconds)", y = "Usage (%)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
    
    plots[[4]] <- ggplot2::ggplot(gpu_metrics, ggplot2::aes(x = x, y = gpu_memory_used_mb)) +
      ggplot2::geom_point(size = 1) +
      ggplot2::labs(title = "GPUM", x = "Time (seconds)", y = "Memory (MB)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
    
    # Combine all 4 plots
    gridExtra::grid.arrange(grobs = plots, nrow = 4)
  } else {
    # Combine only CPU plots
    gridExtra::grid.arrange(grobs = plots, nrow = 2)
  }
}

# Helper function to check if collectl GPU monitoring is available
cl_gpu_exists <- function() {
  cl_exists() && system2("collectl", "-sG", stdout = NULL, stderr = NULL) == 0
}

# Helper function to check if NVML is available
nvml_available <- function() {
  # This would check for NVML library availability
  # Implementation depends on your specific NVML setup
  requireNamespace("Rnvml", quietly = TRUE) || requireNamespace("nvidia", quietly = TRUE)
}