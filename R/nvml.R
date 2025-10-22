#' @useDynLib Rcollectlgpu, .registration = TRUE
NULL

#' Initialize NVML
#' @export
nvml_init <- function() {
  .Call("nvml_init")
}

#' Shutdown NVML
#' @export
nvml_shutdown <- function() {
  .Call("nvml_shutdown")
}

#' Get number of NVIDIA GPUs
#' @export
nvml_device_count <- function() {
  .Call("nvml_device_count")
}

#' Get GPU metrics
#' @param device_index GPU index (0-based)
#' @export
nvml_get_metrics <- function(device_index = 0) {
  .Call("nvml_get_metrics", as.integer(device_index))
}

#' Check if NVML is available
#' @export
nvml_available <- function() {
  tryCatch({
    nvml_init()
    count <- nvml_device_count()
    nvml_shutdown()
    count > 0
  }, error = function(e) FALSE)
}

#' GPU Monitoring Class
#' @export
GPUMonitor <- R6::R6Class("GPUMonitor",
  public = list(
    initialize = function() {
      if (!nvml_available()) {
        stop("NVML not available on this system")
      }
      nvml_init()
      private$device_count <- nvml_device_count()
    },
    
    get_metrics = function(device_index = 0) {
      if (device_index >= private$device_count) {
        stop("Invalid device index")
      }
      nvml_get_metrics(device_index)
    },
    
    get_all_metrics = function() {
      lapply(0:(private$device_count-1), function(i) {
        metrics <- self$get_metrics(i)
        metrics$device_index <- i
        metrics
      })
    },
    
    finalize = function() {
      nvml_shutdown()
    }
  ),
  
  private = list(
    device_count = 0
  )
)