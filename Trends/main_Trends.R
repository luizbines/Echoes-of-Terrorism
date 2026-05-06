# Main script to run all processing scripts in the correct order for Trends

resolve_project_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  repeat {
    if (dir.exists(file.path(current_dir, "Voting")) && dir.exists(file.path(current_dir, "Trends"))) {
      return(current_dir)
    }
    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      break
    }
    current_dir <- parent_dir
  }
  stop("ERROR: Could not determine project root from start_dir=", start_dir)
}

project_root <- Sys.getenv("R_PROJECT_ROOT")
if (nzchar(project_root) && dir.exists(project_root)) {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
} else {
  project_root <- resolve_project_root()
}
base_path <- file.path(project_root, "Trends")

# Verify we're in the right directory
if (!dir.exists(file.path(base_path, "raw")) && !dir.exists(file.path(base_path, "cleaning"))) {
  stop("ERROR: Could not determine Trends directory path. base_path=", base_path)
}

Sys.setenv(R_PROJECT_ROOT = project_root, R_MODULE_ROOT = base_path)

# run mode: "simple" (default), "extraction" or "dry-run" (alias: "dry")
args <- commandArgs(trailingOnly = TRUE)
run_mode <- if (length(args) >= 1) tolower(args[1]) else "simple"
if (run_mode == "dry") {
  run_mode <- "dry-run"
}
if (!run_mode %in% c("simple", "extraction", "dry-run")) {
  warning("Invalid mode '", run_mode, "'. Falling back to 'simple'.")
  run_mode <- "simple"
}
dry_run <- identical(run_mode, "dry-run")

run_scripts_from_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    message("Directory does not exist: ", dir_path)
    return(invisible(NULL))
  }
  scripts <- list.files(path = dir_path, pattern = "\\.(R|py)$", full.names = TRUE, ignore.case = TRUE)
  # exclude hidden files, .Rhistory, and main scripts
  scripts <- scripts[!basename(scripts) %in% c(".", "..", "main.R", "main_Trends.R", "main.py", ".Rhistory")]
  scripts <- scripts[!grepl("^\\.", basename(scripts))]
  if (length(scripts) == 0) {
    return(invisible(NULL))
  }
  # alphabetical, case-insensitive
  scripts <- scripts[order(tolower(basename(scripts)))]
  for (script_path in scripts) {
    cat("\n========================================\n")
    if (dry_run) {
      cat("DRY-RUN (would run):", script_path, "\n")
    } else {
      cat("Running:", script_path, "\n")
    }
    cat("========================================\n")

    if (dry_run) {
      next
    }

    # protect runner functions and important variables in case sourced scripts clear the workspace
    saved_run <- if (exists("run_scripts_from_dir", envir = .GlobalEnv, inherits = FALSE)) get("run_scripts_from_dir", envir = .GlobalEnv) else NULL
    saved_process <- if (exists("process_category", envir = .GlobalEnv, inherits = FALSE)) get("process_category", envir = .GlobalEnv) else NULL
    saved_module_root <- if (exists("module_root", envir = .GlobalEnv, inherits = FALSE)) get("module_root", envir = .GlobalEnv) else NULL
    saved_run_mode <- if (exists("run_mode", envir = .GlobalEnv, inherits = FALSE)) get("run_mode", envir = .GlobalEnv) else NULL
    saved_dry_run <- if (exists("dry_run", envir = .GlobalEnv, inherits = FALSE)) get("dry_run", envir = .GlobalEnv) else NULL

    if (grepl("\\.R$", script_path, ignore.case = TRUE)) {
      tryCatch(
        source(script_path, chdir = TRUE),
        error = function(e) {
          cat("ERROR in", script_path, ":", conditionMessage(e), "\n")
        },
        warning = function(w) {
          cat("WARNING in", script_path, ":", conditionMessage(w), "\n")
        }
      )
    } else if (grepl("\\.py$", script_path, ignore.case = TRUE)) {
      tryCatch({
        status <- system2("python3", args = shQuote(script_path))
        if (!identical(status, 0L)) {
          cat("ERROR in", script_path, ": python3 exited with status", status, "\n")
        }
      }, error = function(e) {
        cat("ERROR in", script_path, ":", conditionMessage(e), "\n")
      })
    }

    # restore runner functions if they were removed by the sourced script
    if (!is.null(saved_run) && !exists("run_scripts_from_dir", envir = .GlobalEnv, inherits = FALSE)) {
      assign("run_scripts_from_dir", saved_run, envir = .GlobalEnv)
    }
    if (!is.null(saved_process) && !exists("process_category", envir = .GlobalEnv, inherits = FALSE)) {
      assign("process_category", saved_process, envir = .GlobalEnv)
    }
    if (!is.null(saved_module_root) && !exists("module_root", envir = .GlobalEnv, inherits = FALSE)) {
      assign("module_root", saved_module_root, envir = .GlobalEnv)
    }
    if (!is.null(saved_run_mode) && !exists("run_mode", envir = .GlobalEnv, inherits = FALSE)) {
      assign("run_mode", saved_run_mode, envir = .GlobalEnv)
    }
    if (!is.null(saved_dry_run) && !exists("dry_run", envir = .GlobalEnv, inherits = FALSE)) {
      assign("dry_run", saved_dry_run, envir = .GlobalEnv)
    }
  }
}

# Process category: discovers subdirectories dynamically and processes them
process_category <- function(category_path, category_name) {
  cat("\n\nProcessing:", category_name, "\n")
  
  if (!dir.exists(category_path)) {
    message("Category path does not exist: ", category_path)
    return(invisible(NULL))
  }
  
  # Get all subdirectories (1 level deep)
  subdirs <- list.dirs(path = category_path, full.names = FALSE, recursive = FALSE)
  # filter out hidden and output directories
  subdirs <- subdirs[!grepl("^\\.|Output$|output$", subdirs)]
  
  if (length(subdirs) == 0) {
    return(invisible(NULL))
  }
  
  # sort alphabetically
  subdirs <- sort(subdirs)
  
  for (subdir in subdirs) {
    subdir_path <- file.path(category_path, subdir)
    
    # possible locations to look for scripts
    paths_to_check <- c(
      file.path(subdir_path, "Code"),
      file.path(subdir_path, "input"),
      file.path(subdir_path, "scripts"),
      subdir_path
    )
    
    found <- FALSE
    for (p in paths_to_check) {
      if (dir.exists(p)) {
        found <- TRUE
        cat("\n---", subdir, " (", p, ") ---\n")
        run_scripts_from_dir(p)
        
        # look for Robustness subdirectory
        robustness_paths <- c(file.path(p, "Robustness"), file.path(subdir_path, "Robustness"))
        for (rp in robustness_paths) {
          if (dir.exists(rp)) {
            cat("\n--- Robustness (", subdir, ") ---\n")
            run_scripts_from_dir(rp)
            break
          }
        }
        break
      }
    }
    
    if (!found) {
      # silently skip subdirectories with no standard script locations
    }
  }
}

process_extraction <- function(base_path) {
  cat("\n\nProcessing: EXTRACTION\n")

  extraction_candidates <- c(
    file.path(base_path, "Extraction"),
    file.path(base_path, "raw", "Extraction"),
    file.path(base_path, "raw", "Google Trends", "Extraction")
  )

  found_any <- FALSE
  for (p in extraction_candidates) {
    if (dir.exists(p)) {
      found_any <- TRUE
      cat("\n--- Extraction (", p, ") ---\n")
      run_scripts_from_dir(p)
    }
  }

  if (!found_any) {
    message("Extraction folder not found (skipped).")
  }
}

# Execute in order: raw, cleaning, treating
# dry-run mirrors extraction flow (lists everything extraction mode would run)
if (run_mode %in% c("extraction", "dry-run")) {
  process_extraction(base_path)
}

process_category(file.path(base_path, "raw"), "RAW DATA")
process_category(file.path(base_path, "cleaning"), "CLEANING")
process_category(file.path(base_path, "treating"), "TREATING")

cat("\nAll done (mode:", run_mode, ").\n")
