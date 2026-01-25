################### INDICAICONES ############################

# CORRER SOLO SI YA SE CORRIO ANEMIAS1 O ANEMIAS_FROM_PENDRIVE, 
# QUE HAYAN GENERADO EL ARCHIVO EVAL_PACIENTES_INC.CSV, SE HAYA SUBIDO A DRIVE,
# UNA PERSONA HAYA PUESTO M O F EN AQUELLOS QUE TIENEN ESTADO = "Si sexo = masculino --> Excluir, Si femenino --> Incluir",
# SE HAYA DESCARGADO ESE ARCHIVO EVALUADO Y SUBIDO A LA CARPETA DEL PROYECTO EN R, 
# AHORA CON EL NOMBRE DE "eval_pacientes_inc - eval_pacientes_inc.csv".


##############################################################

# lo que hace este codigo es tomar ese archivo, leer los pacientes que tenian que ser evaluados, 
# si son hombres con hb menor a 13 les da el estado de Excluidos por anemia basal,
# y reescribimos la tabla con esos estados, con más excluidos.
# luego se evalua de nuevo cuales son los incluidos y excluidos y se mueven los archivos
# de los pacientes que deben ser excluidos a la carpeta de Excluidos (subcarpeta dentro de Pacientes)
# al finalizar esta tarea, elimina las tablas intermedias "eval_pacientes_inc", "eval_pacientes_inc - eval_pacientes_inc.csv" y 
# nombra a la nueva tabla, que fue ademas modificada en cuanto a sus columnas como reevaluado.csv


###############################################################
library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# 1. Configuración de rutas
setwd("~/R Studio/PaulaAnemias")
path_pacientes <- "Pacientes"
path_excluidos <- file.path(path_pacientes, "Excluidos")

if (!dir.exists(path_excluidos)) dir.create(path_excluidos)

# 2. Leer y validar por sexo/hemoglobina
# (Nota: Asegúrate de que 'sexo' y 'hb_inicial' estén en el CSV o se unan aquí)
evaluado <- read_csv("eval_pacientes_inc - eval_pacientes_inc.csv") %>% 
  mutate(estado = if_else(
    sexo == "M" & estado == "Si sexo = masculino --> Excluir, Si femenino --> Incluir",
    paste0("EXCLUIDO - Anemia basal: ", hb_inicial),
    estado
  )) %>% 
  relocate(estado, .after = last_col())

# 3. Mover archivos que no tengan estado "Incluir"
archivos_a_excluir <- evaluado %>% 
  filter(estado != "Incluir") %>% 
  pull(archivo)

for (f in archivos_a_excluir) {
  ruta_origen <- file.path(path_pacientes, f)
  ruta_destino <- file.path(path_excluidos, f)
  
  if (file.exists(ruta_origen)) {
    file.rename(from = ruta_origen, to = ruta_destino)
    message(paste("Movido a Excluidos:", f))
  }
}

# 4. Guardar reporte final 
tabla_seguimiento <- "reevaluado.csv" # conservar: abajo usa la variable tabla seguimiento
write_csv(evaluado, tabla_seguimiento)


# 5. Eliminar temporales viejos
archivos_transitorios <- c("eval_pacientes_inc.csv","eval_pacientes_inc - eval_pacientes_inc.csv" )

if (file.exists(tabla_seguimiento)) {
  existentes <- archivos_transitorios[file.exists(archivos_transitorios)]
  if (length(existentes) > 0) {
    file.remove(as.character(existentes))
    message("Limpieza de archivos transitorios completada.")
  }
} else {
  stop("No se encontró reevaluado.csv. No se borraron los temporales.")
}
