load_common_packages <- function(pkgs, quietly = TRUE) {
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE, quietly = quietly)) {
      stop(
        sprintf(
          "Package '%s' is required. Install it with install.packages('%s').",
          pkg,
          pkg
        ),
        call. = FALSE
      )
    }
  }
}

# Core packages loaded for every document
load_common_packages(
  c(
    "tidyverse",
    # "flexiblas",
    "sdotools",
    "sdodemog",
    "DBI",
    "RPostgres",
    "googledrive"
  )
)

# Configure numerical backend once per render
# configure_flexiblas <- function(backend = "OPENBLAS-OPENMP") {
#   if (!flexiblas_load_backend(backend)) {
#     warning(sprintf("Unable to load FlexiBLAS backend '%s'.", backend))
#   }
#   flexiblas_set_num_threads(max(1, parallel::detectCores() - 1))
# }

# configure_flexiblas()

# Centralized flags for side effects
sdo_allow_writes <- identical(
  tolower(Sys.getenv("SDO_ALLOW_WRITES", "false")),
  "true"
)
sdo_allow_downloads <- identical(
  tolower(Sys.getenv("SDO_ALLOW_DOWNLOADS", "false")),
  "true"
)

# Utility to guard Drive actions
with_drive_access <- function(expr) {
  if (!sdo_allow_downloads) {
    stop(
      "Remote Drive access disabled. Set SDO_ALLOW_DOWNLOADS=true to fetch or upload files.",
      call. = FALSE
    )
  }
  force(expr)
}

# Utility to guard database writes
with_db_writes <- function(expr) {
  if (!sdo_allow_writes) {
    return(invisible(NULL))
  }
  force(expr)
}

options(
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE
)
