#library(officer)
# install.packages("readtext")
# install.packages("stringr")
library(readtext)
library(stringr)
library(readr)
library(tidyverse)

# Configurar rutas
path_word <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/PAU altas/"
path_txt  <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/PAU_altas_txt"

# Crear la carpeta de destino si no existe
if (!dir.exists(path_txt)) dir.create(path_txt)

# 1. Leer todos los archivos .docx de una sola vez
# Esto es mucho más rápido que un loop individual
datos_word <- readtext(paste0(path_word, "*.docx"))

# 2. Guardar cada uno como .txt
# Usamos un loop rápido solo para la escritura en disco
for (i in 1:nrow(datos_word)) {
  # Limpiar el nombre: quitar .docx y poner .txt
  nombre_archivo <- str_replace(datos_word$doc_id[i], "\\.docx$", ".txt")
  
  # Escribir el contenido en la carpeta de destino
  write_lines(datos_word$text[i], file.path(path_txt, nombre_archivo))
}

print(paste("Proceso completado. Se convirtieron", nrow(datos_word), "archivos."))
