source("R/00_config.R")
activate_local_lib()
source("R/01_packages.R")

check_required_packages()
ensure_project_dirs()

# Run in-process for compatibility with restricted environments.
targets::tar_make(callr_function = NULL)
