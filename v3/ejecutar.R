# Session > Set Working Directory > To Source File Location.
setwd("C:/Users/luisa/OneDrive/Documentos/R Studio/PaulaAnemias/v3")

# Source all modules
source("librerias.R")
source("config.R")           # Load config FIRST (has path definitions)
source("patterns.R")
source("validadores.R")
source("checkpoints.R")
source("funciones.R")        # Must come after patterns.R
source("analisis_resultados.R")
source("generar_reporte.R")
source("logger.R")           # Defines setup_logging and extraction_log

#------------
# Set paths from config.R
path_word     <- CONFIG$paths$word
path_txt      <- CONFIG$paths$txt
path_proyecto <- CONFIG$paths$proyecto

# Initialize logging (now that path_proyecto is set)
setup_logging(path_proyecto)

path_pacientes <- file.path(path_proyecto, "Pacientes")
path_excluidos <- file.path(path_pacientes, "Excluidos")

#============================================================================
# EJECUCIÓN DEL FLUJO PASO A PASO
#============================================================================
#============================================================================
# Paso A: Convertir de Word a TXT
#============================================================================
message("--- Iniciando conversión de Word a TXT ---")
docx_a_txt(path_word, path_txt)
#============================================================================
# Paso B: Segmentar el archivo grande en pacientes individuales
#============================================================================
message("--- Iniciando segmentación de pacientes ---")
segmentar_pacientes(path_proyecto, path_pacientes, path_excluidos) 

# Save checkpoint after segmentation
save_checkpoint("segmentation_complete", 
                list(path_pacientes = path_pacientes), 
                path_proyecto)

# Crear el nombre de los archivos analizados para la doc
str_archivos <- paste(basename(list.files(path_txt, pattern = "\\.txt$")), collapse = "; ")

#============================================================================
# Paso C: Evaluación clínica y creación de Tabla Inicial
#============================================================================

message("--- Iniciando evaluación clínica ---")

# Try to resume from checkpoint
tabla_inicial_from_checkpoint <- ask_resume("tabla_inicial", path_proyecto)

if (is.null(tabla_inicial_from_checkpoint)) {
  # No checkpoint found, run evaluation
  tabla_inicial <- evaluar_pacientes(path_pacientes)
  tabla_inicial <- clasificar(tabla_inicial)
  save_checkpoint("tabla_inicial", tabla_inicial, path_proyecto)
} else {
  # Checkpoint found and user accepted
  tabla_inicial <- tabla_inicial_from_checkpoint
}

#============================================================================
# Paso D: Análisis estadístico
#============================================================================

message("\n--- Generando análisis de resultados ---")
analisis_obj <- analizando_resultados(tabla_inicial)

#============================================================================
# Paso E: Generar reporte Excel
#============================================================================

message("\n--- Generando reporte Excel ---")
exportar_reporte_final(
  tabla = tabla_inicial, 
  analisis = analisis_obj, 
  ruta_archivo = file.path(path_proyecto, "Reporte.xlsx"),
  archivos_analizados_str = str_archivos
)

# Save original filenames for workflow 2
saveRDS(str_archivos, file.path(path_proyecto, "archivos_analizados.rds"))

message(paste("\n✅ Reporte generado con éxito en:", path_proyecto))

#============================================================================
# Paso H: Exportar logs
#============================================================================

# Export extraction issues log if any were recorded
if (length(extraction_log) > 0) {
  export_extraction_log()
  message(paste("\n📊 Log de extracción guardado en:", 
                file.path(path_proyecto, "extraction_issues.csv")))
}

message("\n🎉 Ejecución de Workflow 1 completada!\n")