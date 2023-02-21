
get_cores <- function(n_cores = NULL) {
  
  max_cores <- as.integer(
    Sys.getenv("LSB_DJOB_NUMPROC", unset = parallel::detectCores())
  )
  
  if (is.null(n_cores)) {
    n_cores <- max_cores
  }
  
  if (n_cores > max_cores) {
    warning("asked for ", n_cores, " cores but only ", max_cores,
            " are available")
    n_cores <- max_cores
  }
  
  n_cores
}

get_root <- function() {
  rprojroot::find_root(".git/index")
}

json_file <- function(name, dir, value = NULL, simplifyVector = TRUE,
                      simplifyDataFrame = FALSE, simplifyMatrix = FALSE, ...) {
  
  assert_that(dir.exists(dir))
  
  file <- paste0(file.path(dir, name), ".json")
  
  if (!is.null(value)) {
    
    assert_that(is.list(value))
    jsonlite::write_json(value, file, ...)
    
  } else {
    
    if (!file.exists(file)) {
      stop("config file ", basename(file), " does not exists.")
    }
    
    jsonlite::read_json(file, simplifyVector = simplifyVector,
                        simplifyDataFrame = simplifyDataFrame,
                        simplifyMatrix = simplifyMatrix, ...)
  }
}

config <- function(name, value = NULL, ...) {
  json_file(name, file.path(get_root(), "config"), value, ...)
}
