#' Transform collectl and GPU data for visualization
#' @param x data.frame output from cl_parse or cl_collectl_data
#' @param tz character(1) POSIXct time zone code, defaults to "EST"
#' @return data.frame formatted for plotting
vizdf = function(x, tz = "EST") {
  cpu_active = function(x)
    data.frame(tm = as.POSIXct(x$sampdate, tz = tz), xtype = "CPU_MEM",
               pos = "top", value = 100 - x$`CPU_Idle%`, type = "%CPU active")
  mem_used = function(x)
    data.frame(tm = as.POSIXct(x$sampdate, tz = tz), xtype = "CPU_MEM",
               pos = "bot", value = x$MEM_Used, type = "MEM used")
  net_KB_cum = function(x)
    data.frame(tm = as.POSIXct(x$sampdate, tz = tz), xtype = "NET_DSK",
               pos = "top", value = (x$NET_RxKBTot + x$NET_TxKBTot), type = "KB NET")
  dsk_KBwr_cum = function(x)
    data.frame(tm = as.POSIXct(x$sampdate, tz = tz), xtype = "NET_DSK",
               pos = "bot", value = cumsum(x$DSK_WriteKBTot), type = "Cumul KB disk")
  gpu_util = function(x)
    if (exists("gpu_util_gpu0", x)) {
      data.frame(tm = as.POSIXct(x$sampdate, tz = tz), xtype = "GPU",
                 pos = "top", value = x$gpu_util_gpu0, type = "%GPU active")
    } else {
      data.frame()
    }
  gpu_mem_used = function(x)
    if (exists("memory_used_gpu0", x)) {
      data.frame(tm = as.POSIXct(x$sampdate, tz = tz), xtype = "GPU",
                 pos = "bot", value = x$memory_used_gpu0, type = "GPU MEM used")
    } else {
      data.frame()
    }

  rbind(cpu_active(x), mem_used(x), net_KB_cum(x), dsk_KBwr_cum(x),
        gpu_util(x), gpu_mem_used(x))
}

#' Elementary display of usage data from collectl and GPU
#' @import ggplot2
#' @param x output of cl_parse or cl_collectl_data
#' @return ggplot with geom_line and facet_grid
#' @examples
#' \dontrun{
#' proc <- cl_start(tempfile(), monitor_gpu = TRUE, gpu_monitor_type = "nvml")
#' Sys.sleep(5)
#' cl_stop(proc)
#' usage_df <- cl_collectl_data(proc)
#' plot_usage(usage_df)
#' }
#' @export
plot_usage = function(x) {
  ggplot(vizdf(x), aes(x = tm, y = value)) +
    geom_line() +
    facet_grid(vars(type), scales = "free") +
    labs(title = "System and GPU Usage Over Time", x = "Time", y = "Value") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}