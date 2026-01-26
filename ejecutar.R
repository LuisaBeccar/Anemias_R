# Session > Set Working Directory > To Source File Location.
setwd("C:/Users/luisa/OneDrive/Documentos/R Studio/PaulaAnemias/")

source("librerias.R")
source("funciones.R")
source("analisis_resultados.R")
source("generar_reporte.R")
library(dplyr)


#------------
# C:\Users\luisa\OneDrive\Desktop\sup_PEDRIVE\test_modularizado
path_word <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/"
path_txt  <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/"

path_proyecto <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/" # donde estan los archivos grandotes .txt 
path_pacientes <- file.path(path_proyecto, "Pacientes")
path_excluidos <- file.path(path_pacientes, "Excluidos")

# 4. EJECUCIÓN DEL FLUJO PASO A PASO

# Paso A: Convertir de Word a TXT
message("--- Iniciando conversión de Word a TXT ---")
docx_a_txt(path_word,path_txt)

# Paso B: Segmentar el archivo grande en pacientes individuales
message("--- Iniciando segmentación de pacientes ---")
segmentar_pacientes(path_proyecto, path_pacientes, path_excluidos) 

# Paso C: Evaluación clínica y creación de Tabla 1
message("--- Iniciando evaluación clínica ---")
tabla_inicial <- evaluar_pacientes(path_pacientes, path_excluidos)

# 2. Refinamiento por sexo (Interactiva): # Esta función se detendrá y te preguntará en la consola
tabla_final <- solicitar_sexo(tabla_inicial)

# Paso D. Limpieza física de carpetas: # Mueve los archivos .txt según la decisión de la tabla
message("--- Moviendo archivos excluidos a su carpeta de Excluidos ---")
organizar_archivos(tabla_final, path_pacientes, path_excluidos)

###################################

# Analisis de tabla
analisis <- analizando_resultados(tabla_final)
message("--- Generando analisis de resultados ---")

# Crear el nombre de los archivos analizados para la doc (opcional)
str_archivos <- paste(basename(list.files(path_txt, pattern = "\\.txt$")), collapse = "; ")

# generar reporte
exportar_reporte_final(
  tabla = tabla_final, 
  analisis = analisis, 
  ruta_archivo = file.path(path_proyecto, "Reporte_Final.xlsx"),
  archivos_analizados_str = str_archivos
)
