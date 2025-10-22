#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Forward declarations of your C functions
SEXP nvml_init();
SEXP nvml_shutdown();
SEXP nvml_device_count();
SEXP nvml_get_metrics(SEXP device_index);

// Method registration
static const R_CallMethodDef callMethods[] = {
  {"nvml_init", (DL_FUNC) &nvml_init, 0},
  {"nvml_shutdown", (DL_FUNC) &nvml_shutdown, 0},
  {"nvml_device_count", (DL_FUNC) &nvml_device_count, 0},
  {"nvml_get_metrics", (DL_FUNC) &nvml_get_metrics, 1},
  {NULL, NULL, 0}
};

void R_init_Rcollectlgpu(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}