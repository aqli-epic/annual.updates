# Resolve repository files and external inputs without changing the working directory.
# External inputs: set AQLI_DATA_DIR to the folder that holds them.
# If that variable is unset, a historical Desktop path is used when the file is still there.

aqli_repo_root <- function() {
  d <- normalizePath(getwd(), mustWork = FALSE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "annual.updates.Rproj"))) {
      return(d)
    }
    parent <- dirname(d)
    if (identical(parent, d)) {
      break
    }
    d <- parent
  }
  normalizePath(getwd(), mustWork = FALSE)
}

aqli_input <- function(name, repo_rel = NULL, legacy = NULL) {
  candidates <- character()
  data_dir <- Sys.getenv("AQLI_DATA_DIR", unset = "")
  if (nzchar(data_dir)) {
    candidates <- c(candidates, file.path(data_dir, name))
  }
  if (!is.null(repo_rel)) {
    candidates <- c(candidates, file.path(aqli_repo_root(), repo_rel))
  }
  if (!is.null(legacy)) {
    candidates <- c(candidates, path.expand(legacy))
  }
  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }
  stop(
    "Could not find '", name, "'. Set AQLI_DATA_DIR or add the file at one of:\n",
    paste(candidates, collapse = "\n"),
    call. = FALSE
  )
}

aqli_source <- function(repo_rel, legacy = NULL) {
  repo_path <- file.path(aqli_repo_root(), repo_rel)
  if (file.exists(repo_path)) {
    source(repo_path, local = FALSE)
    return(invisible(repo_path))
  }
  if (!is.null(legacy) && file.exists(path.expand(legacy))) {
    source(path.expand(legacy), local = FALSE)
    return(invisible(path.expand(legacy)))
  }
  stop(
    "Could not source '", repo_rel, "'. Expected it in the repo or at ",
    ifelse(is.null(legacy), "(no legacy path)", path.expand(legacy)),
    call. = FALSE
  )
}
