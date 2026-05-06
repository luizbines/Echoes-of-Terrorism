# Master script to run both main_Voting.R and main_Trends.R in the same mode

# Parse run mode from command line arguments
args <- commandArgs(trailingOnly = TRUE)
run_mode <- if (length(args) >= 1) tolower(args[1]) else "simple"
if (run_mode == "dry") {
  run_mode <- "dry-run"
}
if (!run_mode %in% c("simple", "extraction", "dry-run")) {
  warning("Invalid mode '", run_mode, "'. Falling back to 'simple'.")
  run_mode <- "simple"
}

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║          ECHOES OF TERRORISM - MASTER EXECUTOR                 ║\n")
cat("║                                                                ║\n")
cat("║  Mode: ", toupper(run_mode), "\n", sep = "")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Define paths
voting_main <- "/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Voting/main_Voting.R"
trends_main <- "/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Trends/main_Trends.R"

# Function to execute a main script in a given mode
run_main_script <- function(script_path, script_name, mode) {
  if (!file.exists(script_path)) {
    cat("ERROR: Script not found:", script_path, "\n")
    return(FALSE)
  }
  
  cat("\n")
  cat("════════════════════════════════════════════════════════════════\n")
  cat("Running:", script_name, "in", toupper(mode), "mode\n")
  cat("════════════════════════════════════════════════════════════════\n")
  
  # Execute script with mode argument. Show stdout but suppress stderr (child warnings)
  status <- system2("Rscript", args = c(script_path, mode), stdout = "", stderr = "/dev/null")
  
  if (!identical(status, 0L)) {
    cat("WARNING: ", script_name, " exited with status", status, "\n")
    return(FALSE)
  }
  
  return(TRUE)
}

# Execute Voting first, then Trends
voting_ok <- run_main_script(voting_main, "main_Voting.R", run_mode)
trends_ok <- run_main_script(trends_main, "main_Trends.R", run_mode)

# Summary
cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("EXECUTION SUMMARY\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("Mode:", toupper(run_mode), "\n")
cat("main_Voting.R:", if (voting_ok) "✓ OK" else "✗ FAILED", "\n")
cat("main_Trends.R:", if (trends_ok) "✓ OK" else "✗ FAILED", "\n")
cat("════════════════════════════════════════════════════════════════\n")

if (voting_ok && trends_ok) {
  cat("\n✓ All scripts executed successfully!\n\n")
  quit(status = 0)
} else {
  cat("\n✗ Some scripts failed. Check output above.\n\n")
  quit(status = 1)
}