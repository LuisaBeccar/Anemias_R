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

setwd("C:/Users/luisa/OneDrive/Documentos/R Studio/PaulaAnemias/")

library(here)
library(tidyverse)
library(stringi)
library(readr)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(janitor)

here::i_am("decision_pacientes_02.R")
here()

# 1. CONFIGURACIÓN DE RUTAS
#C:\Users\luisa\OneDrive\Desktop\sup_PEDRIVE\PAU_altas_txt
path_proyecto <- "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/PAU_altas_txt/de drive como txt/" # donde estan los archivos grandotes .txt 
path_pacientes <- file.path(path_proyecto, "Pacientes")
path_excluidos <- file.path(path_pacientes, "Excluidos")

# Crear estructura de carpetas si no existe
if (!dir.exists(path_pacientes)) dir.create(path_pacientes, recursive = TRUE)
if (!dir.exists(path_excluidos)) dir.create(path_excluidos, recursive = TRUE)

# 2. LISTAR ARCHIVOS DEL PENDRIVE que sean .txt
archivos_pendrive <- list.files(path = path_proyecto, pattern = "\\.txt$", full.names = TRUE)

# 1. Extraer solo el nombre (ej: "blabla.txt") de la ruta completa para luego documentar
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
        es_inicio = str_detect(X1, "^\\d{4}\\s*-?\\s*[a-zA-ZáéíóúÁÉÍÓÚÑñ\\s]+\\s*(\\(\\d+\\)|\\d{1,2})?"),         #(?:.*?DNI)),   # \\s+(\\d+)(?:años)?\\s*(.*)$"),    # previo "^\\d{4}\\s*[-]?\\s*[A-Za-z].*(\\(\\d+\\))?"),
        id_paciente_unico = cumsum(es_inicio) # 2. ID secuencial único: Cada paciente tendrá su propio número del 1 al 256
      ) %>%
      filter(id_paciente_unico > 0) %>%
      group_by(id_paciente_unico) %>%
      mutate(
        # Extraer el ID de 4 dígitos de la primera fila del grupo
        id_4digitos = str_extract(first(X1), "^\\d{4}"),
        
        # 3. NOMBRADO SEGURO: 
        # Usamos el ID secuencial + el ID de cama (////+ clave aleatoria
        # Ejemplo: 001_3301_Ke6X7.txt
        indice_str = sprintf("%03d", id_paciente_unico), # Convierte 1 en 001
       # clave_aleatoria = stri_rand_strings(1, 5),  ## quizas no es necesario.. 
        
        nombre_archivo = paste0(indice_str, "_", id_4digitos,  ".txt") # opcional "_", clave_aleatoria,
      ) %>%
      ungroup()
    
    # 4. Verificación antes de guardar
    print(paste("Pacientes detectados:", length(unique(df_segmentado$nombre_archivo))))
    
    
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
  #if (p == "nombre.extencion de tabla final") next
  
  ruta_origen <- file.path(path_pacientes, p)
  ruta_destino <- file.path(path_excluidos, p)
  
  contenido <- read_lines(ruta_origen)
  if (length(contenido) < 2) next 
  
  # --- Extracción de edad, nombre de primera linea 
  linea_1 <- contenido[1]
  
  edad_raw <- linea_1 %>% 
    str_remove("^\\d{4}\\s*[-]?\\s*") %>%
    str_remove("\\s*[-]?\\s*.") %>% 
    str_extract("(?i)(\\(\\s*(edad\\s*)?\\d[\\s\\d]{1,3}(.*?)\\)|\\bedad[:\\s]*\\d[\\s\\d]{1,3}\\b|\\b\\d[\\s\\d]{1,3}\\s*(años?|ños|a|(?=\\s*-?DNI|\\s*-?HC|$)))")
   
   # [\\s\\d]{1,3} capturar números que tienen espacios locos en medio
   #  detectará la edad  antes de "DNI", y sino también antes de palabras como "años", "HC" o simplemente antes de una letra.


  # 2. Limpiamos solo los números y convertimos de forma segura
  edad <- if (!is.na(edad_raw)) {
    as.numeric(str_remove_all(edad_raw, "[^0-9]")) 
  } else {NA_real_}
  
  nombre_paciente <- linea_1 %>%
    str_replace("^\\d{4}\\s*[-]?\\s*", "") %>%  # Eliminamos los 4 dígitos iniciales y cualquier guion/espacio que les siga
    str_extract("(?i)^.*?(?=\\s*\\(\\s*(edad|\\d+)|\\s+edad|\\s+\\d+|\\s*(DNI|HC|FI)|$)") %>% 
    str_replace(("-"), "") %>% # sacar comas flotando por ahi y que separen nombres
    str_replace((","), "") %>% 
    str_trim() %>%
    toupper()

  # Verificación para el log:
  if (is.na(nombre_paciente) || nombre_paciente == "") {
    nombre_paciente <- "Nombre_No_Detectado"
  }  
  
  #######
  
  
  # Definir la función fuera del loop para mayor eficiencia
  extraer_dato_clinico <- function(texto, etiqueta) {
    # (?i)          : No distingue mayúsculas/minúsculas
    # [:\\s]*       : Acepta ":" y espacios (opcionales) entre etiqueta y valor
    # ([\\d/\\.]+)  : Captura números, barras (fechas) y puntos (nros HC complejos)
    # (?=\\s*(DNI|HC|FI|FICM|,|-|\\s|$)) : Lookahead para frenar en la siguiente etiqueta o signo
    
    patron <- paste0("(?i)", etiqueta, "[:\\s]*([\\d/\\.]+)")
    match <- str_match(texto, patron)
    
    if (!is.na(match[1, 2])) {
      return(str_trim(match[1, 2]))
    } else {
      return(NA_character_)
    }
  }
  
  
  # --- Extraccion de dni, hc, fechas de internacion del bloque clinico
  #bloque_clinico <- paste(contenido[1:min(5, length(contenido))], collapse = " ")
  #
  # extraer_flexible <- function(texto, etiqueta) {
  #   pattern <- if (etiqueta %in% c("DNI:", "HC:")) paste0(etiqueta, "\\s*(\\d+)") else
  #     if (etiqueta %in% c("FI:", "FICM:")) paste0(etiqueta, "\\s*([\\d/]+)") else
  #       paste0(etiqueta, "\\s*([^\\s]+)")
  #   match <- str_match(texto, pattern)
  #   if (!is.na(match[1,2])) return(str_trim(match[1,2])) else return(NA_character_)
  # }
  # 
  # dni_v <- extraer_flexible(bloque_clinico, "DNI:")
  # hc_v  <- extraer_flexible(bloque_clinico, "HC:")
  # fi_v  <- extraer_flexible(bloque_clinico, "FI:")
  # ficm_v <- extraer_flexible(bloque_clinico, "FICM:")
  # 
  
  # Unificamos el bloque de búsqueda (Líneas 1 a 5)
  bloque_clinico <- paste(contenido[1:min(5, length(contenido))], collapse = " ")
  
  # Captura de variables con la nueva lógica
  dni_v  <- extraer_dato_clinico(bloque_clinico, "DNI")
  hc_v   <- extraer_dato_clinico(bloque_clinico, "HC")
  fi_v   <- extraer_dato_clinico(bloque_clinico, "FI")     # Capturará fechas como 25/01/2026
  ficm_v <- extraer_dato_clinico(bloque_clinico, "FICM")
  
  
  
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
  
  # CAMBIO AQUÍ: Verificar que idx_lab NO sea NA antes de crear el rango
  if (!is.na(idx_lab)) {
    # Usar max() para asegurar que idx_lab + 1 no sea menor que 1 (opcional pero seguro)
    limite_superior <- min(idx_lab + 10, length(contenido))
    rango_busqueda <- (idx_lab + 1):limite_superior
    
    for (i in rango_busqueda) {
      linea_lab <- contenido[i]
      
      if (str_detect(linea_lab, "^\\d{1,2}/\\d{1,2}:")) {
        fecha_hb_inicial <- str_extract(linea_lab, "^\\d{1,2}/\\d{1,2}")
        
        datos_puros <- str_remove(linea_lab, "^\\d{1,2}/\\d{1,2}:") %>% str_trim()
        partes <- str_split_1(datos_puros, "/")
        
        if (length(partes) >= 2) {
          valor_candidato <- as.numeric(str_extract(partes[2], "\\d+\\.?\\d*"))
          if (!is.na(valor_candidato) && valor_candidato >= 3.0 && valor_candidato <= 45.0) {
            hb_inicial <- valor_candidato
            break 
          }
        }
      }
      # Si encuentras una línea vacía o el inicio de otra sección, detén la búsqueda
      if (linea_lab == "" || str_detect(linea_lab, "^[A-Z][a-z]+:")) break
    }
  }
  
  # LÓGICA DE CLASIFICACIÓN: comentarios por criterio de exclusion ---

  comentario <- case_when(
    !is.na(edad) && edad < 18                  ~ "< 18 años",
    !is.na(hallazgo_normalizado)               ~ hallazgo_normalizado,
    !is.na(hb_inicial) && hb_inicial < 12.0    ~ "anemia basal",
    !is.na(hb_inicial) && hb_inicial >= 12.0 && hb_inicial < 13 ~ "evaluar sexo",
    is.na(hb_inicial)                          ~ "falta hb inicial",
    TRUE                                       ~ "apto"
  )
  
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
    decision = NA_character_
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

tabla_3 <- tabla_2 %>% 
  mutate(
    # Usamos case_when para múltiples condiciones vectorizadas
    comentario = case_when(
      sexo == "M" & comentario == "evaluar sexo" ~ paste0("anemia basal"),
      sexo == "F" & comentario == "evaluar sexo" ~ "apto",
      TRUE                                     ~ comentario # Mantiene el valor original si no cumple lo anterior
    ),
    # Usamos if_else para una decisión binaria simple
    decision = if_else(comentario != "apto", "EXCLUIR", "CONTINUAR")
  ) %>% 
  relocate(comentario, .after = sexo) %>% 
  relocate(decision, .after = last_col())

########### Mover archivos segun decision

# Anotar pacientes que decision = EXLUIR osea  que no es CONTINUAR
archivos_a_excluir <- tabla_3 %>% 
  filter(decision != "CONTINUAR") %>% 
  pull(archivo)  #agarra los nombres de los archivos en la memoria

# --- mover esos archivos a carpter Excluidos
for (f in archivos_a_excluir) {
  ruta_origen <- file.path(path_pacientes, f)
  ruta_destino <- file.path(path_excluidos, f)
  
  if (file.exists(ruta_origen)) {
    file.rename(from = ruta_origen, to = ruta_destino)
    message(paste("Movido a Excluidos:", f))
  }
}


## --------- analisis de tabla_3

nombres_dni_distintos <- n_distinct(tabla_3$nombre, tabla_3$dni)
nombres_distintos <- n_distinct(tabla_3$nombre)
archivos_distintos <- n_distinct(tabla_3$archivo)

continuan <- tabla_3 %>% filter(decision == "CONTINUAR") %>% count()
excluidos <- tabla_3 %>% filter(decision == "EXCLUIR") %>% count()

summary(tabla_3) %>% View()

colus <- colnames(tabla_3)
# Opción correcta y rápida
vacios <- data.frame(
  columna = colus, 
  cantidad_vacios = colSums(is.na(tabla_3)), 
  row.names = NULL
)

## creo una tabla con comentario normalizado y sin algunas variables
resumir_tabla_3 <- tabla_3 %>% 
  mutate(comentario_normalizado = str_remove(comentario, ",.*$")) %>% 
  select(-c("archivo","nombre", "dni","nro_hc","f_internacion","fi_clinica_medica","f_hb_inicial","comentario"))

## va
grupos_comentarios <- tabla_3 %>% 
  filter(decision =="EXCLUIR") %>% 
  mutate(comentario_normalizado = str_remove(comentario, ",.*$")) %>%
  group_by(comentario_normalizado) %>% 
  count(comentario_normalizado) %>% 
  ungroup() %>% 
  janitor::adorn_totals(where = "row", name = "TOTAL EXCLUIDOS")

## hb_iniciales totales y luego en anemia y no anemia
hb_tot <- edades_tot <- tabla_3 %>%   # aca estaria bueno si tuvieramos todos los sexos
  filter(!is.na(hb_inicial)) %>% 
  summarise(across(hb_inicial,
                   list(min = min,
                        max = max,
                        media = mean, 
                        mediana = median,
                        sd = sd), 
                   .names = "{.col}_{.fn}"))

anemias_basales <- tabla_3 %>%   # aca estaria bueno si tuvieramos todos los sexos
  filter(decision =="EXCLUIR") %>% 
  filter(!is.na(hb_inicial)) %>% 
  mutate(comentario_normalizado = str_remove(comentario, ",.*$")) %>%
  filter(comentario_normalizado == "anemia basal") %>% 
  summarise(across(hb_inicial,
            list(min = min,
                 max = max,
                 media = mean, 
                 mediana = median,
                 sd = sd), 
                 .names = "{.col}_{.fn}"))

sin_anemia <- tabla_3 %>%   # aca estaria bueno si tuvieramos todos los sexos
  filter(decision =="CONTINUAR" ) %>% 
  filter(!is.na(hb_inicial)) %>% 
#  mutate(hb_inicial = as.numeric(hb_inicial)) %>% 
  summarise(across(hb_inicial,
                   list(min = min,
                        max = max,
                        media = mean, 
                        mediana = median,
                        sd = sd), 
                   .names = "{.col}_{.fn}"))

## edades de todos los pacientes
edades_tot <- tabla_3 %>%   # aca estaria bueno si tuvieramos todos los sexos
  filter(!is.na(edad)) %>% 
  summarise(across(edad,
                   list(min = min,
                        max = max,
                        media = mean, 
                        mediana = median,
                        sd = sd), 
                   .names = "{.col}_{.fn}"))


## probando paquete biostats

bio_s <- biostats::summary_table(resumir_tabla_3)

bio <- as.data.frame(bio_s) %>% select(-normality)

excluidos2 <- as.data.frame(biostats::summary_table(resumir_tabla_3, 
                                                    group_by = "decision", 
                                                    all = T,  #??
                                                    exclude = "sexo"))

############# ----- DOCUMENTACION 
valor_nombre_dni_distintos <- as.character(nombres_dni_distintos[[1]]) 
definiciones <- tribble(
  ~Concepto , ~Definicion,
  "Documentacion de la tabla:", "",
  "Archivos analizados:", as.character(archivos_analizados),
  "nro de sub-archivos analizados:", as.character(archivos_distintos),
  "","",
  "", "",
  "Hoja:","Tabla_pacientes",
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
  "decision","Si tiene algun comentario, que son motivos de exclusión, decision será EXLUIR, si no hay comentarios: CONTINUAR",
  "","",
  "","",
  "Algunos números:", "",
  "Cantidad de pacientes (nombre) distintos:", as.character(nombres_distintos),
  "Cantidad de pacientes (nombre + dni) distintos:", valor_nombre_dni_distintos,
  "","",
  "Registro con decisiÓn: EXLCUIR:", as.character(excluidos),
  "Registros con decisiÓn: CONTINUAR:", as.character(continuan),
  "","",
  "","",
  "Resto de hojas:", "analisis de resultados",
  "vacíos:","Numero de registros vacíos en cada variable",
  "biostats 1 y 2:","Tablas creada con paquete Biostats",
  "excluidos","Cantidad de archivos etiquetados con cada causa de exclusion",
  "hb_iniciales:", "Análisis resumen de todas las hb_inciciales (todos los archivos)",
  "anemias basales:", "Análisis resumen de los valores de hb_iniciales en el grupo de pacientes etiquetados con anemia basal",
  "sin anemia:", "Análisis resumen de los valores de hb_iniciales en el grupo de pacientes con decision de CONTINUAR"
  )


######### reporte completo

# Solapas para reporte en xlsx
reporte_completo <- list(
  "definiciones" = definiciones,
  "tabla_pacientes" = tabla_3,
  "vacíos" = vacios,
  "biostats_1" = bio,
  "biostats_2" = excluidos2,
  "excluidos" = grupos_comentarios,
  "hb_iniciales" = hb_tot,
  "anemias_basales" = anemias_basales,
  "sin_anemia" = sin_anemia) 

# crear archivo y guardar en path_proyecto
ruta_destino2 <- file.path(path_proyecto, "reporte_seleccion.xlsx") #ruta completa hacia la carpeta PaulaAnemias (file.path se encarga de poner las barras / correctamente)
write_xlsx(reporte_completo, ruta_destino2)




##########################
#escenario de prueba

