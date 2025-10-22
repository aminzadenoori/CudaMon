#' parse collectl output and optionally GPU metrics
#' @importFrom lubridate as_datetime
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows left_join
#' @param path character(1) path to (possibly gzipped) collectl output
#' @param gpu_metrics_path character(1) path to GPU metrics JSONL file, or NULL
#' @param tz character(1) POSIXct time zone code, defaults to "EST"
#' @return a data.frame with collectl data and optionally GPU metrics
#' @note A lubridate datetime is added as a column. GPU metrics are merged if provided.
#' @examples
#' \dontrun{
#' proc <- cl_start(tempfile(), monitor_gpu = TRUE, gpu_monitor_type = "nvml")
#' Sys.sleep(5)
#' cl_stop(proc)
#' usage_df <- cl_parse(cl_result_path(proc), cl_gpu_metrics_path(proc))
#' }
#' @export
cl_parse = function(path, gpu_metrics_path = NULL, tz = "EST") {
  # Parse collectl output
  full = readLines(path)
  inds = grep("^####", full)
  stopifnot(length(inds) == 2)
  lastm = inds[2]
  meta = readLines(path)[seq_len(lastm)]
  dat = read.delim(path, skip = lastm, check.names = FALSE, sep = " ")
  names(dat) = gsub("\\[(...)\\]", "\\1_", names(dat))
  dat = revise_date(dat, tz = tz)
  attr(dat, "meta") = meta
  
  # Parse GPU metrics if provided
  if (!is.null(gpu_metrics_path) && file.exists(gpu_metrics_path)) {
    gpu_data = lapply(readLines(gpu_metrics_path), jsonlite::fromJSON)
    gpu_df = dplyr::bind_rows(gpu_data)
    gpu_df$timestamp = lubridate::as_datetime(gpu_df$timestamp, tz = tz)
    
    # Merge GPU metrics with collectl data based on nearest timestamp
    dat$timestamp = dat$sampdate
    gpu_df = gpu_df[order(gpu_df$timestamp), ]
    dat = dat[order(dat$timestamp), ]
    
    # Approximate merge by finding the nearest timestamp
    merged = dat
    for (i in seq_len(nrow(dat))) {
      time_diff = abs(difftime(dat$timestamp[i], gpu_df$timestamp, units = "secs"))
      closest = which.min(time_diff)
      if (length(closest) > 0 && time_diff[closest] < 2) { # 2-second tolerance
        for (col in c("gpu_util", "mem_util", "temperature", "power_usage", "memory_used", "memory_total")) {
          merged[i, paste0(col, "_gpu", gpu_df$device_index[closest])] = gpu_df[[col]][closest]
        }
      }
    }
    dat = merged
  }
  
  dat
}

revise_date = function(x, tz) {
  c2 = paste(x[,1], x[,2])
  pred = gsub("(....)(..)(..)(.*)", "\\1-\\2-\\3\\4", c2)
  x$sampdate = lubridate::as_datetime(pred, tz = tz)
  x
}