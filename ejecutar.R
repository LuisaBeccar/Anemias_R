# Session > Set Working Directory > To Source File Location.
setwd("C:/Users/luisa/OneDrive/Documentos/R Studio/PaulaAnemias")

source("librerias.R")
source("funciones.R")
source("analisis_resultados.R")
source("generar_reporte.R")


#------------
# C:\Users\luisa\OneDrive\Desktop\sup_PEDRIVE\test_modularizado
path_word <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/"
path_txt  <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/"

path_proyecto <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado" # donde estan los archivos grandotes .txt 
path_pacientes <- file.path(path_proyecto, "Pacientes")
path_excluidos <- file.path(path_pacientes, "Excluidos")

# 4. EJECUCIÓN DEL FLUJO PASO A PASO

# Paso A: Convertir de Word a TXT
message("--- Iniciando conversión de Word a TXT ---")
docx_a_txt(path_word,path_txt)

# Paso B: Segmentar el archivo grande en pacientes individuales
message("--- Iniciando segmentación de pacientes ---")
segmentar_pacientes(path_proyecto, path_pacientes, path_excluidos) 


# Crear el nombre de los archivos analizados para la doc (opcional)
str_archivos <- paste(basename(list.files(path_txt, pattern = "\\.txt$")), collapse = "; ")

# Paso C: Evaluación clínica y creación de Tabla 1
message("--- Iniciando evaluación clínica ---")
tabla_inicial <- evaluar_pacientes(path_pacientes, path_excluidos)

# 2. Refinamiento por sexo (Interactiva): # Esta función se detendrá y te preguntará en la consola
tabla_final <- solicitar_sexo(tabla_inicial)

###################################

# Analisis de tabla
analisis_obj <- analizando_resultados(tabla_final)
message("--- Generando analisis de resultados ---")

# generar reporte
exportar_reporte_final(
  tabla = tabla_final, 
  analisis = analisis_obj, 
  ruta_archivo = file.path(path_proyecto, "Reporte.xlsx"),
  archivos_analizados_str = str_archivos
)

message(paste("--- Reporte generado con éxito en ",path_proyecto," ---"))

# Pregunta interactiva al usuario
cat("\n------------------------------------------------------------------\n")
respuesta <- readline(prompt = "¿Desea editar la tabla manualmente para completar sexos antes de mover archivos? (Y/N): ")
cat("------------------------------------------------------------------\n")

if (toupper(respuesta) == "Y") {
  
  # OPCIÓN Y: El usuario quiere el proceso largo
  message("\n[ACCIÓN] Por favor, abra 'Reporte_Final.csv', complete los sexos faltantes en la hoja 'Tabla_Pacientes',")
  message("Luego tiene dos opciones: ")
  message(" A) guárdarlo como csv llamandola 'Tabla_Pacientes', metala en la carpeta del proyecto y ejecute el script 'ejecutar_2.R'.")
  message(" B) en Google drive convertirlo a formato nativo (Archivo: Guardar como hoja de calculo de Google. Abrirlo, editar los sexos. copiar el url. Se usara en el script Ejecutar 2" )
  message("El programa finalizará ahora sin mover archivos físicos.\n")
  
} else if (toupper(respuesta) == "N") {
  
  # OPCIÓN N: El usuario quiere terminar ahora
  message("\n[ACCIÓN] Procediendo a organizar archivos con la información actual...")
  
  # Movemos los archivos según la decisión de tabla_final (la automática)
  organizar_archivos(tabla_final, path_pacientes, path_excluidos)
  
  message("--- Proceso finalizado con éxito (sin reevaluación manual) ---")
  
} else {
  message("\n[ERROR] Opción no válida. No se realizaron movimientos de archivos.")
}
