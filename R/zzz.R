if (FALSE){

# R/zzz.R
.onLoad <- function(libname, pkgname) {
  # Load the shared library
  library.dynam("cudamatrix", pkgname, libname)
  
  # Check if CUDA is available
  if (!cuda_available()) {
    packageStartupMessage("Warning: CUDA is not available on this system")
  } else {
    packageStartupMessage("cudamatrix loaded successfully - CUDA available")
  }
}

.onUnload <- function(libpath) {
  # Unload the shared library when package is unloaded
  library.dynam.unload("cudamatrix", libpath)
}

# Helper function to check CUDA availability
cuda_available <- function() {
  tryCatch({
    # Try to call a simple function from your shared library
    # You might want to create a simple CUDA availability check function
    .Call("cuda_available", PACKAGE = "cudamatrix")
    TRUE
  }, error = function(e) {
    FALSE
  })
}
}