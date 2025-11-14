#include <R.h>
#include <Rinternals.h>
#include <nvml.h>

// Initialize NVML
SEXP nvml_init() {
    nvmlReturn_t result = nvmlInit_v2(); // Use v2 for better compatibility
    if (result != NVML_SUCCESS) {
        Rprintf("%s\n", nvmlErrorString(result));
        Rf_error("NVML Init failed: %s", nvmlErrorString(result));
    }
    Rprintf("Sucess");
    return R_NilValue;
}

// Shutdown NVML
SEXP nvml_shutdown() {
    nvmlReturn_t result = nvmlShutdown();
    if (result != NVML_SUCCESS) {
        Rf_error("NVML Shutdown failed: %s", nvmlErrorString(result));
    }
    return R_NilValue;
}

// Get device count
SEXP nvml_device_count() {
    unsigned int count;
    nvmlReturn_t result = nvmlDeviceGetCount(&count);
    if (result != NVML_SUCCESS) {
        Rf_error("Failed to get device count: %s", nvmlErrorString(result));
    }
    return ScalarInteger(count);
}

// Get GPU metrics for a specific device
SEXP nvml_get_metrics(int device_index) {
    device_index = 0;
    nvmlDevice_t device;
    nvmlReturn_t result;
    
    // Check if NVML is initialized
    unsigned int count;
    result = nvmlDeviceGetCount(&count);
    if (result == NVML_ERROR_UNINITIALIZED) {
        result = nvmlInit_v2(); // Try to initialize
        if (result != NVML_SUCCESS) {
            Rf_error("NVML initialization failed: %s", nvmlErrorString(result));
        }
        result = nvmlDeviceGetCount(&count); // Retry
        if (result != NVML_SUCCESS) {
            Rf_error("Failed to get device count: %s", nvmlErrorString(result));
        }
    } else if (result != NVML_SUCCESS) {
        Rf_error("Failed to get device count: %s", nvmlErrorString(result));
    }
    
    // Validate device index
    if (device_index < 0 || device_index >= (int)count) {
        Rf_error("Invalid device index: %d. Must be between 0 and %d", device_index, count - 1);
    }
    
    // Get device handle
    result = nvmlDeviceGetHandleByIndex(device_index, &device);
    if (result != NVML_SUCCESS) {
        Rf_error("Failed to get device handle: %s", nvmlErrorString(result));
    }
    
    // Get metrics
    nvmlUtilization_t utilization;
    unsigned int temp = NA_INTEGER;
    unsigned int power = NA_INTEGER;
    nvmlMemory_t memory_info = {0}; // Initialize to avoid garbage values
    
    result = nvmlDeviceGetUtilizationRates(device, &utilization);
    if (result != NVML_SUCCESS) {
        utilization.gpu = NA_INTEGER;
        utilization.memory = NA_INTEGER;
    }
    
    result = nvmlDeviceGetTemperature(device, NVML_TEMPERATURE_GPU, &temp);
    // No need for explicit error handling; NA_INTEGER already set
    
    result = nvmlDeviceGetPowerUsage(device, &power);
    // No need for explicit error handling; NA_INTEGER already set
    
    result = nvmlDeviceGetMemoryInfo(device, &memory_info);
    if (result != NVML_SUCCESS) {
        memory_info.used = NA_REAL;
        memory_info.total = NA_REAL;
    }
    
    // Create named list to return
    SEXP metrics = PROTECT(allocVector(VECSXP, 6));
    SEXP names = PROTECT(allocVector(STRSXP, 6));
    
    SET_STRING_ELT(names, 0, mkChar("gpu_util"));
    SET_STRING_ELT(names, 1, mkChar("mem_util"));
    SET_STRING_ELT(names, 2, mkChar("temperature"));
    SET_STRING_ELT(names, 3, mkChar("power_usage"));
    SET_STRING_ELT(names, 4, mkChar("memory_used"));
    SET_STRING_ELT(names, 5, mkChar("memory_total"));
    
    SET_VECTOR_ELT(metrics, 0, ScalarInteger(utilization.gpu));
    SET_VECTOR_ELT(metrics, 1, ScalarInteger(utilization.memory));
    SET_VECTOR_ELT(metrics, 2, ScalarInteger(temp));
    SET_VECTOR_ELT(metrics, 3, ScalarInteger(power));
    SET_VECTOR_ELT(metrics, 4, ScalarReal(memory_info.used / 1024.0 / 1024.0)); // Convert to MB
    SET_VECTOR_ELT(metrics, 5, ScalarReal(memory_info.total / 1024.0 / 1024.0)); // Convert to MB
    
    setAttrib(metrics, R_NamesSymbol, names);
    UNPROTECT(2);
    
    return metrics;
}

// R interface function with input validation
SEXP R_nvml_get_metrics(SEXP device_index) {
    if (!isInteger(device_index) && !isReal(device_index)) {
        Rf_error("Device index must be an integer");
    }
    int idx = asInteger(device_index);
    if (idx == NA_INTEGER) {
        Rf_error("Invalid device index: NA not allowed");
    }
    return nvml_get_metrics(idx);
}