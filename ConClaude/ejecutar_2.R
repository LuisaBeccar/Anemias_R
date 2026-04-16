# ==============================================================================
# SCRIPT DE REEVALUACIÓN FINAL (POST-EDICIÓN DE EXCEL)
# ==============================================================================

source("librerias.R")
source("config.R")
source("patterns.R")
source("funciones.R")
source("analisis_resultados.R")
source("generar_reporte_2.R")
source("generar_reporte.R")

# Rutas
path_proyecto  <- CONFIG$paths$proyecto
path_pacientes <- file.path(path_proyecto, "Pacientes")
path_excluidos <- file.path(path_pacientes, "Excluidos")

# 1. Leer la tabla que completaste manualmente opcion A o B

#  Asegúrate de que el nombre coincida con tu archivo editado
archivo_input <- file.path(path_proyecto, "Tabla_Pacientes.csv")


if (file.exists(archivo_input)) {
  
  # Leemos la tabla (ajusta el delimitador si usas  o CSV)
  tabla_revisada <- readr::read_delim(archivo_input, delim = ",", locale = locale(decimal_mark = ",")) 
  tabla_revisada <- tabla_revisada %>% 
    mutate(edad = as.integer(edad),
           hb_inicial = as.numeric(hb_inicial))
  #tabla_revisada <- archivo_input
  message("--- Iniciando Reevaluación de Datos Completos ---")
  
  # 2. Correr la función de revisión (la que combina comentarios)
  tabla_ok <- revision_tabla(tabla_revisada)
  
  # # 3. Nuevo análisis de resultados (actualiza métricas de Hb y conteos)
  # analisis_final <- analizando_resultados(tabla_ok)
  message("\n--- Generando análisis de resultados ---")
  analisis_final <- analizando_resultados(tabla_ok)
  
  # # 4. Generar el Reporte Final definitivo
  # str_archivos <- paste(basename(list.files(path_proyecto, pattern = "\\.txt$")), collapse = "; ")
  # message("\n--- Generando reporte Excel ---")
  exportar_reporte_final_2(
    tabla = tabla_ok, 
    analisis = analisis_final, 
    ruta_archivo = file.path(path_proyecto, "Reporte_Final.xlsx"),
    archivos_analizados_str = str_archivos
  )
  
  # 5. Movimiento físico de archivos
  # Si tras completar el sexo alguien que era APTO ahora es ANEMIA, se mueve a Excluidos
  message("--- Organizando archivos en carpetas finales ---")
  organizar_archivos(tabla_ok, path_pacientes, path_excluidos)
  
  # # Save original filenames for workflow 2
  # saveRDS(str_archivos_2, file.path(path_proyecto, "archivos_analizados.rds"))
  
  message(paste("\n✅ Reporte generado con éxito en:", path_proyecto))
  
  
} else {
  stop("No se encontró el archivo: ", archivo_input)
}
