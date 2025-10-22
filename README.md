# CudaMon: GPU Monitoring for R

Extended System and GPU Monitoring with Collectl and NVML

![Repo Logo](https://github.com/aminzadenoori/Rcollectl-GPU/blob/develgpu/cudamon.jpeg?raw=true)
## Quick Start

```r
library(CudaMon)

# Start monitoring with GPU tracking
proc <- cl_start("my_workflow", monitor_gpu = TRUE, gpu_monitor_type = "nvml")

# Your GPU computation here
cl_timestamp(proc, "computation_start")
# ... your code ...
cl_timestamp(proc, "computation_end")

# Stop and visualize
cl_stop(proc)
cl_plot_system_metrics(proc)  # 4-panel CPU/GPU plot
```

## Features

- **System Monitoring**: CPU, memory, disk I/O via collectl
- **GPU Monitoring**: Two backends:
  - `collectl -sG` (basic)
  - NVML (NVIDIA GPUs - detailed metrics)
- **Background Collection**: Continuous GPU metrics during computation
- **Integrated Plots**: Combined CPU/GPU usage visualization

## Installation

```r
devtools::install_github("aminzadenoori/CudaMon")
```

**System Requirements**: collectl utility, NVIDIA drivers (for NVML)

## API

- `cl_start()` / `cl_stop()` - Start/stop monitoring
- `cl_timestamp()` - Mark workflow phases  
- `cl_get_gpu_metrics()` - Access real-time GPU data
- `cl_plot_system_metrics()` - Generate usage plots

## Example Output

![CPU/GPU Monitoring Plot](https://github.com/aminzadenoori/Rcollectl-GPU/blob/develgpu/image%20(1).png)

Shows CPU usage, memory, GPU utilization, and GPU memory over time.

---

*Extends Rcollectl with GPU monitoring capabilities for profiling accelerated computations.*