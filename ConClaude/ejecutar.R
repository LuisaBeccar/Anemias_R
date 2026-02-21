# Session > Set Working Directory > To Source File Location.
setwd("C:/Users/luisa/OneDrive/Documentos/R Studio/PaulaAnemias/ConClaude")

# Source all modules
source("librerias.R")
source("config.R")           # Load config FIRST (has path definitions)
source("patterns.R")
source("validadores.R")
source("checkpoints.R")
source("funciones.R")        # Must come after patterns.R
source("analisis_resultados.R")
source("generar_reporte.R")
source("test_extraction.R")

# NOW source logger (after config is loaded so path_proyecto exists)
# But we need to set path_proyecto first!

#------------
# Set paths (can eventually come from config.R)
path_word <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/"
path_txt  <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/"
path_proyecto <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado"
# Initialize logging (now that path_proyecto is set)
setup_logging(path_proyecto)

path_pacientes <- file.path(path_proyecto, "Pacientes")
path_excluidos <- file.path(path_pacientes, "Excluidos")

# NOW we can source logger (path_proyecto exists now)
source("logger.R")

#============================================================================
# 4. EJECUCIÓN DEL FLUJO PASO A PASO
#============================================================================

# Paso A: Convertir de Word a TXT
message("--- Iniciando conversión de Word a TXT ---")
docx_a_txt(path_word, path_txt)

# Paso B: Segmentar el archivo grande en pacientes individuales
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
  tabla_inicial <- evaluar_pacientes(path_pacientes, path_excluidos)
  save_checkpoint("tabla_inicial", tabla_inicial, path_proyecto)
} else {
  # Checkpoint found and user accepted
  tabla_inicial <- tabla_inicial_from_checkpoint
}

# Preview extraction results
message("\n--- Mostrando preview de datos extraídos ---")
preview_extraction(tabla_inicial, n_rows = 50)

response <- readline(prompt = "\n¿Los datos se ven correctos? Continue with analysis? (Y/N): ")
if (toupper(response) != "Y") {
  message("\n[INFO] Pipeline detenido por el usuario después del preview.")
  message("[INFO] Puede revisar los datos y volver a ejecutar.")
  stop("Pipeline stopped by user after preview", call. = FALSE)
}

#============================================================================
# Paso D: Refinamiento por sexo (Interactivo)
#============================================================================

message("\n--- Iniciando validación de sexo ---")

# Try to resume from checkpoint
tabla_final_from_checkpoint <- ask_resume("tabla_final", path_proyecto)

if (is.null(tabla_final_from_checkpoint)) {
  # No checkpoint, run sex validation
  tabla_final <- solicitar_sexo(tabla_inicial)
  save_checkpoint("tabla_final", tabla_final, path_proyecto)
} else {
  # Checkpoint found and accepted
  tabla_final <- tabla_final_from_checkpoint
}

#============================================================================
# Paso E: Análisis estadístico
#============================================================================

message("\n--- Generando análisis de resultados ---")
analisis_obj <- analizando_resultados(tabla_final)

#============================================================================
# Paso F: Generar reporte Excel
#============================================================================

message("\n--- Generando reporte Excel ---")
exportar_reporte_final(
  tabla = tabla_final, 
  analisis = analisis_obj, 
  ruta_archivo = file.path(path_proyecto, "Reporte.xlsx"),
  archivos_analizados_str = str_archivos
)

# Save original filenames for workflow 2
saveRDS(str_archivos, file.path(path_proyecto, "archivos_analizados.rds"))

message(paste("\n✅ Reporte generado con éxito en:", path_proyecto))

#============================================================================
# Paso G: Decisión del usuario - ¿Edición manual o finalizar?
#============================================================================

cat("\n====================================================================\n")
cat("OPCIONES:\n")
cat("  Y - Editar tabla manualmente (continuar con ejecutar_2.R después)\n")
cat("  N - Finalizar ahora y organizar archivos\n")
cat("====================================================================\n")

respuesta <- readline(prompt = "¿Desea editar la tabla manualmente? (Y/N): ")
cat("====================================================================\n\n")

if (toupper(respuesta) == "Y") {
  
  # OPCIÓN Y: Proceso largo con edición manual
  message("[ACCIÓN] Para continuar:")
  message("")
  message("1. Abra 'Reporte.xlsx'")
  message("2. Vaya a la hoja 'Tabla_Pacientes'")
  message("3. Complete los sexos faltantes manualmente")
  message("4. Exporte como CSV o edite en Google Sheets")
  message("")
  message("Opciones para continuar:")
  message("  A) Guardar como 'Tabla_Pacientes.csv' en la carpeta del proyecto")
  message("  B) Subir a Google Sheets y obtener el URL")
  message("")
  message("5. Ejecute 'ejecutar_2.R' cuando esté listo")
  message("")
  message("⚠️  El programa finalizará ahora sin mover archivos.\n")
  
} else if (toupper(respuesta) == "N") {
  
  # OPCIÓN N: Finalizar ahora
  message("\n[ACCIÓN] Procediendo a organizar archivos con la información actual...\n")
  
  # Organizar archivos según decisiones
  organizar_archivos(tabla_final, path_pacientes, path_excluidos)
  
  message("\n✅ Proceso finalizado con éxito (sin reevaluación manual)\n")
  
} else {
  message("\n❌ Opción no válida. No se realizaron movimientos de archivos.")
  message("Ejecute el script nuevamente y elija Y o N.\n")
}

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