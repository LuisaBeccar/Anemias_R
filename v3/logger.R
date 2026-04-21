library(logger)

# Setup logging function (to be called AFTER path_proyecto is set)
setup_logging <- function(path_proyecto) {
  log_threshold(INFO)
  log_appender(appender_tee(file.path(path_proyecto, "pipeline.log")))
  log_info("=== Pipeline logging initialized ===")
}

# Initialize extraction log
extraction_log <- list()

log_extraction_issue <- function(archivo, campo, valor_extraido, linea_original) {
  issue <- list(
    timestamp = Sys.time(),
    archivo = archivo,
    campo = campo,
    valor = valor_extraido,
    linea = linea_original,
    es_NA = is.na(valor_extraido)
  )
  
  extraction_log[[length(extraction_log) + 1]] <<- issue
  
  if (is.na(valor_extraido)) {
    log_warn("Failed to extract {campo} from {archivo}")
  }
}

# Export log at end
export_extraction_log <- function() {
  if (length(extraction_log) == 0) {
    log_info("No extraction issues to export")
    return(invisible(NULL))
  }
  
  df <- bind_rows(extraction_log)
  write_csv(df, file.path(path_proyecto, "extraction_issues.csv"))
  
  # Summary
  log_info("Total extraction issues: {nrow(df)}")
  log_info("Missing edad: {sum(df$campo == 'edad' & df$es_NA, na.rm = TRUE)}")
  log_info("Missing hb_inicial: {sum(df$campo == 'hb_inicial' & df$es_NA, na.rm = TRUE)}")
}