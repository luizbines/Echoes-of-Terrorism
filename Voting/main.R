# ...existing code...

base_path <- "/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Voting"

run_scripts_from_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    message("Directory does not exist: ", dir_path)
    return(invisible(NULL))
  }
  scripts <- list.files(path = dir_path, pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)
  # exclude hidden files and this main script if accidentally inside
  scripts <- scripts[!basename(scripts) %in% c(".", "..", "main.R")]
  if (length(scripts) == 0) {
    message("No R scripts found in: ", dir_path)
    return(invisible(NULL))
  }
  # alphabetical, case-insensitive
  scripts <- scripts[order(tolower(basename(scripts)))]
  for (script_path in scripts) {
    cat("\n========================================\n")
    cat("Running:", script_path, "\n")
    cat("========================================\n")
    tryCatch(
      source(script_path, chdir = TRUE),
      error = function(e) {
        cat("ERROR in", script_path, ":", conditionMessage(e), "\n")
      },
      warning = function(w) {
        cat("WARNING in", script_path, ":", conditionMessage(w), "\n")
      }
    )
  }
}

# ...existing code...

process_category <- function(category_path, category_name) {
  cat("\n\nProcessing:", category_name, "\n")
  subdirs_order <- c("Red Alerts", "Israel", "Elections")
  for (subdir in subdirs_order) {
    # possible locations to look for scripts
    paths_to_check <- c(
      file.path(category_path, subdir, "Code"),
      file.path(category_path, subdir)
    )
    found <- FALSE
    for (p in paths_to_check) {
      if (dir.exists(p)) {
        found <- TRUE
        cat("\n---", subdir, " (", p, ") ---\n")
        run_scripts_from_dir(p)
        # look for Robustness inside this path (either p/Robustness)
        robustness_paths <- c(file.path(p, "Robustness"), file.path(category_path, subdir, "Robustness"))
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
      message("Subdirectory not found (skipped): ", file.path(category_path, subdir))
    }
  }
}

# Execute in order: raw, cleaning, treating
process_category(file.path(base_path, "raw"), "RAW DATA")
process_category(file.path(base_path, "cleaning"), "CLEANING")
process_category(file.path(base_path, "treating"), "TREATING")

cat("\nAll done.\n")

# ...existing code...