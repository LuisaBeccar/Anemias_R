##--------------------  CONSIDERACIONES LEER ANTES DE EJECUTAR ------------------------------------
##
## cada vez que se lean los archivos se creara un nombre distinto para cada 
## paciente que se forma por el nro de cama. ej 3301, _ y un codigo random 
## alfanumerico de 5 caracteres
##
##-------------------  IMPORTANTE ----------------------------------------- 
##
## ---------  CORRER EL ARCHIVO DESDE R
##
## comentar las lineas 205 y 207 y descomentar las 203 204
## no uses el botón Source normal
##
## Usa la flecha pequeña junto al botón Source (ARRIBA A LA DERECHA) y selecciona 
##
##          "Source with Echo" 
##     (o presiona Ctrl+Shift+Enter)
##  
##    Esto obliga a R a procesar línea por línea y esperar a que contestes 
##        por cada paciente que hay que decidir
##
##   queda: sexo --> masculino = 1, 
##                   solo si es necesario para el flujo, 
##
##    VER SI REQUIERE HACER EL PROCESO PARA TODOS CAMBIAR EL CODIGO O NO 
##    ###########
##
## ----------  CORRER EL ARCHIVO DESDE POWERSHELL CONSOLA WINDOWS
##
## descomentar lineas 205 207 y comentar las 203 y 204 
## abrir consola windows X seleccionar Terminal
## con cd.. ir hascta directori C
## pegar: & "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" --verbose "C:\Users\luisa\OneDrive\Documentos\R Studio\PaulaAnemias\decision_pacientes.R" --interactive
##
## atenti a que te pregunta el sexo de algunos pacientes Y/N, escribis la opcion y enter para cada uno
##
##
################# observacion #######################
##
## el ultimo paciente del archivo queda con el 
## "✨MODELO DRIVE CREAR COPIA ✨
## y todo eso pegado abajo
## si molesta vere de sacarlo, pero por ahora no parece molestar 
##
################################################################################
###############
library(tidyverse)
library(stringi)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# 1. CONFIGURACIÓN DE RUTAS

path_proyecto <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE"  # "~/R Studio/PaulaAnemias"
path_pacientes <- file.path(path_proyecto, "Pacientes")
path_excluidos <- file.path(path_pacientes, "Excluidos")

# Crear estructura de carpetas si no existe
if (!dir.exists(path_pacientes)) dir.create(path_pacientes, recursive = TRUE)
if (!dir.exists(path_excluidos)) dir.create(path_excluidos, recursive = TRUE)

# 2. LISTAR ARCHIVOS DEL PENDRIVE (Ej: 3300.txt, 3400.txt...). El patrón busca archivos que empiecen con 4 dígitos y sean .txt
archivos_pendrive <- list.files(path = path_proyecto, pattern = "^\\d{4}.*\\.txt$", full.names = TRUE)
# 1. Extraer solo el nombre (ej: "3300.txt") de la ruta completa para luego documentar
nombres_limpios <- basename(archivos_pendrive)

# 2. Unir todos los nombres en un solo string separados por " - "
archivos_analizados <- paste(nombres_limpios, collapse = "; ") # elegir el separador que mas te guste


# --- INICIO DEL PROCESO DE SEGMENTACIÓN ---

for (archivo_txt in archivos_pendrive) {
  
  # Leer el archivo del pendrive
  temp_data <- read_delim(archivo_txt, delim = "None", col_names = FALSE, show_col_types = FALSE) %>%
    rename(X1 = 1)
  
  # Identificar pacientes y asignar nombres únicos
  df_segmentado <- temp_data %>%
    mutate(
      es_inicio = str_detect(X1, "^\\d{4} - "),
      id_4digitos = if_else(es_inicio, str_extract(X1, "^\\d{4}"), NA_character_)
    ) %>%
    fill(id_4digitos, .direction = "down") %>%
    filter(!is.na(id_4digitos)) %>%
    group_by(id_4digitos) %>%
    mutate(
      # Generar clave única de 5 caracteres por cada grupo de 4 dígitos
      clave_unica = stri_rand_strings(1, 5),
      nombre_archivo = paste0(id_4digitos, "_", clave_unica, ".txt")
    ) %>%
    ungroup()
  
  # Guardar cada subarchivo en la carpeta Pacientes del proyecto local
  df_segmentado %>%
    group_split(nombre_archivo) %>%
    walk(~ write_lines(.x$X1, file.path(path_pacientes, unique(.x$nombre_archivo))))
  
  message(paste("Segmentado y guardado:", basename(archivo_txt)))
}

# --- INICIO DEL PROCESO DE EVALUACIÓN de cada archivo-paciente ---

# Ahora listamos los archivos individuales generados en la carpeta local:Solo archivos .txt, ignorando carpetas (si existe ya la de Excluidos)
pacientes <- list.files(path = path_pacientes, pattern = "\\.txt$", full.names = FALSE, recursive = FALSE)

regex_exclusion <- "(embaraz|gestac|hemorrag|politrauma|trauma(?!to|log|tolo))"
registro_inicial <- list()

for (p in pacientes) {
  # IMPORTANTE: Evitar procesar archivos que ya están en la carpeta
  if (p == "tabla_decision.csv") next
  
  ruta_origen <- file.path(path_pacientes, p)
  ruta_destino <- file.path(path_excluidos, p)
  
  contenido <- read_lines(ruta_origen)
  if (length(contenido) < 2) next 
  
  # --- Extracción de edad, nombre de primera linea 
  linea1 <- contenido[1]
  edad <- as.numeric(str_extract(linea1, "(?<=\\()\\d+(?=\\))"))
  nombre_paciente <- str_extract(linea1, "(?<= - ).*") %>% 
    str_remove("\\s*\\(\\d+\\).*$") %>% str_trim()
  
  # --- Extraccion de dni, hc, fechas de internacion del bloque clinico
  bloque_clinico <- paste(contenido[2:min(5, length(contenido))], collapse = " ")
  
  extraer_flexible <- function(texto, etiqueta) {
    pattern <- if (etiqueta %in% c("DNI:", "HC:")) paste0(etiqueta, "\\s*(\\d+)") else
      if (etiqueta %in% c("FI:", "FICM:")) paste0(etiqueta, "\\s*([\\d/]+)") else
        paste0(etiqueta, "\\s*([^\\s]+)")
    match <- str_match(texto, pattern)
    if (!is.na(match[1,2])) return(str_trim(match[1,2])) else return(NA_character_)
  }
  
  dni_v <- extraer_flexible(bloque_clinico, "DNI:")
  hc_v  <- extraer_flexible(bloque_clinico, "HC:")
  fi_v  <- extraer_flexible(bloque_clinico, "FI:")
  ficm_v <- extraer_flexible(bloque_clinico, "FICM:")
  
  # --- Detectar criterios clinicos de Exclusión ---
  texto_completo <- tolower(paste(contenido, collapse = " "))
  hallazgo_exclusion <- str_extract(texto_completo, regex_exclusion)

  # Normalización del hallazgo
  hallazgo_normalizado <- case_when(
    str_detect(hallazgo_exclusion, "embaraz|gestac") ~ "embarazo",
    str_detect(hallazgo_exclusion, "hemorrag")      ~ "hemorragia",
    str_detect(hallazgo_exclusion,"politrauma|trauma(?!to|log|tolo)") ~ "traumatismo"
  )
  
  # --- BLOQUE DE LABORATORIO (1era Hb y su Fecha) 
  idx_lab <- which(str_detect(contenido, regex("laboratorios:", ignore_case = TRUE)))[1]
  hb_inicial <- NA_real_
  fecha_hb_inicial <- NA_character_
  
  if (length(idx_lab) > 0) {
    rango_busqueda <- (idx_lab + 1):min(idx_lab + 10, length(contenido))
    for (i in rango_busqueda) {
      linea_lab <- contenido[i]
      if (str_detect(linea_lab, "^\\d{1,2}/\\d{1,2}:")) {
        # Extraer la fecha (lo que está antes de los dos puntos)
        fecha_hb_inicial <- str_extract(linea_lab, "^\\d{1,2}/\\d{1,2}")
        
        datos_puros <- str_remove(linea_lab, "^\\d{1,2}/\\d{1,2}:") %>% str_trim()
        partes <- str_split_1(datos_puros, "/")
        if (length(partes) >= 2) {
          valor_candidato <- as.numeric(str_extract(partes[2], "\\d+\\.?\\d*"))
          if (!is.na(valor_candidato) && valor_candidato >= 3.0 && valor_candidato <= 30.0) {
            hb_inicial <- valor_candidato
            break 
          }
        }
      }
      if (linea_lab == "" || str_detect(linea_lab, "^[A-Z][a-z]+:")) break
    }
  }
  
  # LÓGICA DE CLASIFICACIÓN: comentarios por criterio de exclusion ---
  comentario <- NULL
  if (!is.na(edad) && edad < 18) {
    comentario <- "< 18 años"
  } else if (!is.na(hallazgo_normalizado)) {
    comentario <- paste(hallazgo_normalizado)
  } else if (!is.na(hb_inicial) && hb_inicial < 12.0) {
    comentario <- paste("anemia basal, hb:", hb_inicial)
  } else if (!is.na(hb_inicial) && hb_inicial >= 12.0 && hb_inicial <13) {
    comentario <- "evaluar sexo"
  }
  
  # --- Etiquetar decision
  if (!is.null(comentario) && comentario == "evaluar sexo") {
    decision <- "a evaluar"
  } else if  (!is.null(comentario)) {
      decision <- "EXCLUIR"
    } else {decision <- "CONTINUAR"
  }
  
  
  # --- GUARDAR TODO EN TABLA
  registro_inicial[[p]] <- tibble(
    archivo = p,
    nombre = nombre_paciente,
    edad = edad,
    dni = dni_v,
    nro_hc = hc_v,
    f_internacion = fi_v,
    fi_clinica_medica = ficm_v,
    f_hb_inicial = fecha_hb_inicial,
    hb_inicial = hb_inicial,
    comentario = comentario,
    decision = decision
  )
}

# Consolidar tabla 1
tabla_1 <- bind_rows(registro_inicial)



##### --- Proceso interactivo para definir sexo de los que hb >=12 y <13, sin otro criterio ---

# --- Crear la columna 'sexo' inicializada como NA
tabla_2 <- tabla_1 %>%
  mutate(sexo = NA_real_)

# --- Bucle interactivo para los casos "evaluar sexo"
for (i in 1:nrow(tabla_2)) {
  if (isTRUE(tabla_2$comentario[i] == "evaluar sexo")){

    # # EN R
    # Mostrar la pregunta en consola
    # EN R STUDIO
     mensaje <- paste0("¿Es ", tabla_2$nombre[i], " un paciente de sexo masculino? (Y/N): ")
     respuesta <- readline(prompt = mensaje)

    # # EN CONSOLA
#     cat(paste0("¿Es ", tabla_2$nombre[i], " un paciente de sexo masculino? (Y/N): "))
#     # Forzar la lectura desde la terminal abierta
#     respuesta <- readLines(con = file("stdin"), n = 1)
#     
    # Procesar respuesta (convertimos a mayúsculas para evitar errores)
    if (toupper(respuesta) == "Y") {
      tabla_2$sexo[i] <- "M"
    } else if (toupper(respuesta) == "N") {
      tabla_2$sexo[i] <- "F" # O el código que prefieras para femenino
    } else {
      message("Respuesta no válida, se mantendrá como NA")
    }
  }
} 
# Agrega el mensaje aquí:
cat("\n--------------------------------------------------------------\n")
cat("Gracias por completar la revision del sexo de pacientes con hb basal menor a 13 pero mayora o igual a 12")
cat("\n--------------------------------------------------------------\n")
cat("Continando el anailis")

#####################################################################

# --- Leer y validar por sexo/hemoglobina

# (Nota: Asegúrate de que 'sexo' y 'hb_inicial' estén en el CSV o se unan aquí)
# tabla_3 <- tabla_2 %>% 
#   mutate(
#     comentario = if(sexo == 1 & comentario == "evaluar sexo"){
#                              paste0("anemia basal: ", hb_inicial)
#       } else if (sexo == 0 & comentario == "evaluar sexo"){
#         ""} else paste0("comentario"),
#     decision = if_else(!is.na(comentario), "EXCLUIR", "CONTINUAR")
#     ) %>% 
#   relocate(decision, .after = last_col())
# 
tabla_3 <- tabla_2 %>% 
  mutate(
    # Usamos case_when para múltiples condiciones vectorizadas
    comentario = case_when(
      sexo == "M" & comentario == "evaluar sexo" ~ paste0("anemia basal: ", hb_inicial),
      sexo == "F" & comentario == "evaluar sexo" ~ "",
      TRUE                                     ~ comentario # Mantiene el valor original si no cumple lo anterior
    ),
    # Usamos if_else para una decisión binaria simple
    decision = if_else(comentario != "" & !is.na(comentario), "EXCLUIR", "CONTINUAR")
  ) %>% 
  relocate(decision, .after = last_col())
########### Mover archivos segun decision

# Anotar pacientes que decision = EXLUIR osea  que no es CONTINUAR
archivos_a_excluir <- tabla_3 %>% 
  filter(decision != "CONTINUAR") %>% 
  pull(archivo)

# --- mover esos archivos a carpter Excluidos
for (f in archivos_a_excluir) {
  ruta_origen <- file.path(path_pacientes, f)
  ruta_destino <- file.path(path_excluidos, f)
  
  if (file.exists(ruta_origen)) {
    file.rename(from = ruta_origen, to = ruta_destino)
    message(paste("Movido a Excluidos:", f))
  }
}

### ---- Guardar reporte final 

nombre_archivo_csv <- "tabla_decision.csv" # nombre del archivo
ruta_destino <- file.path(path_proyecto, nombre_archivo_csv) #ruta completa hacia la carpeta PaulaAnemias (file.path se encarga de poner las barras / correctamente)
write_csv(tabla_3, ruta_destino) # guarda el archivo en la ruta destino

# Mensaje de confirmación
cat("Reporte guardado exitosamente en:", ruta_destino)


############# ----- documentar tabla

definiciones <- tribble(
  ~Concepto , ~Definicion,
  "Documentacion de la tabla:", paste0(" ",nombre_archivo_csv),
  "Archivos analizados:", archivos_analizados,
  "","",
  "Variables", "",
  "archivo", "Nombre de archivo: compuesto por el número de cama del paciente, guión bajo, y un código alfanumérico único y rándom de 5 caracteres, que es generado cada vez que se corre el código",
  "nombre", "Nombre y apellido del paciente (o apellido y nombre) como esté escrito en el archivo",
  "edad","Edad en años, registrada en el archivo (número entre paréntesis al lado del nombre)",
  "dni","Número de DNI registrado en el archivo",
  "nro_hc","Nro de historia clínica: 'HC, registrada en el archivo",
  "f_internacion","Fecha de internación: 'FI', registrada en el archivo",
  "fi_clinica_medica","Fecha de internación en clínica mpedica: 'FICM', registrada en el archivo",
  "f_hb_inicial","Fecha del primer laboratorio, del cual se extre el valor de hb_inicial",
  "hb_inicial","Valor de hb obtenido del primer laboratorio: encontrado justo después del titulo 'LABORATORIO:', tomando el segundo valor de la cadena de valores separados por barras",
  "comentario","Comentarios que hacen a la exclusión del paciente de continuar la evaluación para el estudio. En orden de jerarquia, los valores posibles son: '< 18': si la edad del paciente no es mayor o igual a 18; 'hemorragia','traumatismo' o 'embarazo': si se detectó alguna de estos término en el archivo del paciente. 'anemia basal: valor' si se detectó una hb_inicial menor a 12, y luego de la validacion de sexo, tambien si es masculino y su hb_inicial fue menor a 13",
  "sexo","Luego de la evaluacion aparecerá un M (de masculino) o F (de femenino), en aquellos pacientes que requirieron validacion para confirmar si es anemia basal (M<13) o no (F<12). En el caso masculino, el comentario se actualiza a ánemia basal. El femenino quedará sin comentario.",
  "decision","Si tiene algun comentario, que son motivos de exclusión, decision será EXLUIR, si no hay comentarios: CONTINUAR"
)
documentacion_csv <- "documentacion.csv"
ruta_destino2 <- file.path(path_proyecto, documentacion_csv) #ruta completa hacia la carpeta PaulaAnemias (file.path se encarga de poner las barras / correctamente)
write_csv(definiciones, ruta_destino2) # guarda el archivo en la ruta destino

# Mensaje de confirmación
cat("Documentacion guardada exitosamente en:", ruta_destino)
