library(tidyverse)
library(stringi)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# 1. CONFIGURACIÓN DE RUTAS
path_pendrive <- "~/R Studio/PaulaAnemias/"
#path_pendrive <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE"  # Cambiar por ruta del pendrive conectado: ojo con las barras / asi tienen que estar
# para probar copie 3 veces el archivo que tenia y asi obtuve 3300, 3301 y 3302, que son iguales: va a aparecer 3 veces cada paciente 
#pero con nombre de archivo distinto porque le puse el random segunda parte del nombre.txt

path_proyecto <- "~/R Studio/PaulaAnemias/"
path_pacientes <- file.path(path_proyecto, "Pacientes")
path_excluidos <- file.path(path_pacientes, "Excluidos")

# Crear estructura de carpetas si no existe
if (!dir.exists(path_pacientes)) dir.create(path_pacientes, recursive = TRUE)
if (!dir.exists(path_excluidos)) dir.create(path_excluidos, recursive = TRUE)

# 2. LISTAR ARCHIVOS DEL PENDRIVE (Ej: 3300.txt, 3400.txt...)
# El patrón busca archivos que empiecen con 4 dígitos y sean .txt
archivos_pendrive <- list.files(path = path_pendrive, pattern = "^\\d{4}.*\\.txt$", full.names = TRUE)

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

# --- INICIO DEL PROCESO DE EVALUACIÓN (LOOP FOR PAULAANEMIAS) ---

# Ahora listamos los archivos individuales generados en la carpeta local
# Solo archivos .txt, ignorando carpetas (si existe ya la de Excluidos)
pacientes <- list.files(path = path_pacientes, pattern = "\\.txt$", full.names = FALSE, recursive = FALSE)

regex_exclusion <- "(embaraz|gestac|hemorrag|politrauma|trauma(?!to|log|tolo))"
registro_inicial <- list()

for (p in pacientes) {
  # IMPORTANTE: Evitar procesar archivos que ya están en la carpeta (como la tabla final si existiera)
  if (p == "tabla_final.csv") next
  
  ruta_origen <- file.path(path_pacientes, p)
  ruta_destino <- file.path(path_excluidos, p)
  
  contenido <- read_lines(ruta_origen)
  if (length(contenido) < 2) next 
  
  # Extracción Metadatos
  linea1 <- contenido[1]
  edad <- as.numeric(str_extract(linea1, "(?<=\\()\\d+(?=\\))"))
  nombre_paciente <- str_extract(linea1, "(?<= - ).*") %>% 
    str_remove("\\s*\\(\\d+\\).*$") %>% str_trim()
  
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
  
  # --- B. DETECCIÓN CLÍNICA (Exclusión) ---
  texto_completo <- tolower(paste(contenido, collapse = " "))
  hallazgo_exclusion <- str_extract(texto_completo, regex_exclusion)
  
  # --- C. BLOQUE DE LABORATORIO (Hb y Fecha) ---
  idx_lab <- which(str_detect(contenido, regex("laboratorios:", ignore_case = TRUE)))
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
  
  # --- D. LÓGICA DE CLASIFICACIÓN Y MOVIMIENTO ---
  motivo_elim <- NULL
  if (!is.na(edad) && edad < 18) {
    motivo_elim <- "< 18 años"
  } else if (!is.na(hallazgo_exclusion)) {
    motivo_elim <- paste("", toupper(hallazgo_exclusion))
  } else if (!is.na(hb_inicial) && hb_inicial < 12.0) {
    motivo_elim <- paste("Anemia basal, hb:", hb_inicial)
  }
  
  # Determinar estado y mover si es necesario
  if (!is.null(motivo_elim)) {
    estado_final <- paste("EXCLUIDO -", motivo_elim)
    # decido que ahora no se muevan los archivos sino que despuesde segundo analisis todos juntos
    # if (file.exists(ruta_origen)) file.rename(ruta_origen, ruta_destino)
  } else {
    estado_final <- if_else(!is.na(hb_inicial) && hb_inicial < 13.0,
                            "Si sexo = masculino --> Excluir, Si femenino --> Incluir", "Incluir")
  }
  
  # --- E. GUARDAR TODO EN REGISTRO ---
  registro_inicial[[p]] <- tibble(
    archivo = p,
    nombre = nombre_paciente,
    edad = edad,
    dni = dni_v,
    nro_hc = hc_v,
    f_internacion = fi_v,
    fi_clinica_medica = ficm_v,
    f_1era_hb = fecha_hb_inicial,
    hb_inicial = hb_inicial,
    estado = estado_final
  )
}

# 2. Consolidar tabla final

tabla_1 <- bind_rows(registro_inicial)
tabla_1 <- tabla_1 %>% 
  mutate(sexo = "") # le agrego la columna sexo para que quien lo abra sepa donde completar

write_delim(tabla_1, "eval_pacientes_inc.csv",",") # lo guardo en PaulaAnemias

message("1er Proceso terminado. Cargar el csv a Drive o Excel y completar con M o F el sexo de aquellos pacientes con 'Si sexo = masculino --> Excluir, Si femenino --> Incluir'. ",
        "Luego descargar el archivo (sin cambiarle el nombre, se deberia descargar llamandose 'eval_pacientes_inc - eval_pacientes_inc.csv'. ",
        "Por ultimo, subir ese archivo a la carpeta de PaulaAnemias y correr el segundo_analisis.r")
