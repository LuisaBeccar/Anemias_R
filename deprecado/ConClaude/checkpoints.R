# Create new file: checkpoints.R

save_checkpoint <- function(stage_name, data, path_proyecto) {
  checkpoint_dir <- file.path(path_proyecto, "checkpoints")
  if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir)
  
  checkpoint_file <- file.path(checkpoint_dir, paste0(stage_name, ".rds"))
  saveRDS(data, checkpoint_file)
  
  # Save metadata
  metadata <- list(
    stage = stage_name,
    timestamp = Sys.time(),
    rows = nrow(data),
    cols = ncol(data)
  )
  saveRDS(metadata, file.path(checkpoint_dir, paste0(stage_name, "_meta.rds")))
  
  log_info("Checkpoint saved: {stage_name}")
}

load_checkpoint <- function(stage_name, path_proyecto) {
  checkpoint_file <- file.path(path_proyecto, "checkpoints", paste0(stage_name, ".rds"))
  
  if (file.exists(checkpoint_file)) {
    log_info("Loading checkpoint: {stage_name}")
    return(readRDS(checkpoint_file))
  } else {
    log_warn("No checkpoint found for: {stage_name}")
    return(NULL)
  }
}

ask_resume <- function(stage_name, path_proyecto) {
  checkpoint <- load_checkpoint(stage_name, path_proyecto)
  
  if (!is.null(checkpoint)) {
    meta <- readRDS(file.path(path_proyecto, "checkpoints", paste0(stage_name, "_meta.rds")))
    
    cat("\n========================================\n")
    cat("CHECKPOINT FOUND:", stage_name, "\n")
    cat("Created:", as.character(meta$timestamp), "\n")
    cat("Rows:", meta$rows, "| Cols:", meta$cols, "\n")
    cat("========================================\n")
    
    response <- readline(prompt = "Resume from this checkpoint? (Y/N): ")
    
    if (toupper(response) == "Y") {
      return(checkpoint)
    }
  }
  
  return(NULL)
}