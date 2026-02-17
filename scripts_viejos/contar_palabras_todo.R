
library(stringr)

# 1. Definir la carpeta y listar archivos
carpeta <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/PAU_altas_txt"
archivos <- list.files(path = carpeta, pattern = "*.txt", full.names = TRUE)

# 2. Función para contar palabras en un archivo
contar_palabras <- function(archivo) {
  texto <- paste(readLines(archivo, warn = FALSE), collapse = " ")
  # Cuenta palabras usando espacios como separador
  str_count(texto, "\\S+") 
}


a <- list() # Para guardar el texto de cada archivo
palabras <- list() # Para guardar el número de palabras por archivo

for (i in archivos) {
  # 1. Leer el archivo (una línea por elemento del vector)
  temp_text <- readLines(i)
  
  # 2. Unir líneas y separar por espacios para obtener palabras
  todas_las_palabras <- unlist(strsplit(paste(temp_text, collapse = " "), "\\s+"))
  
  # 3. Eliminar posibles espacios vacíos resultantes del split
  todas_las_palabras <- todas_las_palabras[todas_las_palabras != ""]
  
  # 4. Contar palabras y guardar en la lista
  n <- length(todas_las_palabras)
  palabras[[i]] <- n
  
  # Opcional: guardar el texto completo
  a[[i]] <- paste(temp_text, collapse = " ")
}

# Ver el conteo por archivo
print(palabras)

# Ver el total de palabras de todos los archivos
print(sum(unlist(palabras)))
# 19 archivos por ahora
# 1289257 palabras total
# promedio 67855 por archivo
# min 3500, max 160000
