library(dplyr)

# 1 -----------------------------------------------------------
docx_a_txt <- function (ruta_archivos_docx, ruta_destino_txt) {
  if (!dir.exists(ruta_destino_txt)) dir.create(ruta_destino_txt)
  
  datos_word <- readtext(paste0(ruta_archivos_docx, "*.docx"))   # Leer todos los archivos .docx de una sola vez
  
  for (i in 1:nrow(datos_word)) {        # 2. Guardar cada uno como .txt          # Usamos un loop rápido solo para la escritura en disco
    nombre_archivo <- str_replace(datos_word$doc_id[i], "\\.docx$", ".txt")       # Limpiar el nombre: quitar .docx y poner .txt
    write_lines(datos_word$text[i], file.path(ruta_destino_txt, nombre_archivo))  # Escribir el contenido en la carpeta de destino
  }
}

# 2 -------------------------------------------------------------
segmentar_pacientes <- function(path_proyecto, path_pacientes, path_excluidos) {
  
  if (!dir.exists(path_pacientes)) dir.create(path_pacientes, recursive = TRUE)
  if (!dir.exists(path_excluidos)) dir.create(path_excluidos, recursive = TRUE)
  
  archivos_pendrive <- list.files(path = path_proyecto, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(archivos_pendrive) == 0) {
    stop("No se encontraron archivos .txt en la ruta especificada.")
  }
  
  # Acumula el mapeo archivo -> origen a lo largo de todos los .txt procesados
  mapa_origen <- tibble::tibble(archivo = character(), origen = character())
  
  #  contador global de pacientes
  contador_global <- 0L
  # 
  
  pb_seg <- progress_bar$new(
    format = "  Segmenting [:bar] :current/:total files",
    total = length(archivos_pendrive),
    clear = FALSE
  )
  
  for (archivo_txt in archivos_pendrive) {
    pb_seg$tick()
    
    # Nombre del .txt fuente (sin ruta) — esto es el "origen"
    nombre_origen <- basename(archivo_txt)
    
    temp_data <- readr::read_delim(archivo_txt, delim = "\n", col_names = "X1",
                                   show_col_types = FALSE, quote = "")
    
    lineas <- temp_data$X1
    n <- length(lineas)
    
    # --- NUEVA LÓGICA DE DETECCIÓN ---
    # Una línea es inicio de paciente si:
    # 1. Empieza con 4 dígitos seguidos de texto alfabético (patrón original)
    # 2. Y dentro de las 3 líneas siguientes aparece al menos una etiqueta clínica
    
    PATRON_INICIO_CANDIDATO <- "^\\d{4}\\s*-?\\s*[a-zA-ZáéíóúÁÉÍÓÚÑñ]"
    PATRON_ETIQUETAS        <- "(?i)(DNI|\\bHC\\b|\\bFI\\b|FICM)"
    
    es_inicio <- logical(n)
    
    for (i in seq_len(n)) {
      if (stringr::str_detect(lineas[i], PATRON_INICIO_CANDIDATO)) {
        # Ventana de validación: línea actual + 3 siguientes
        ventana_fin <- min(i + 3, n)
        ventana     <- paste(lineas[i:ventana_fin], collapse = " ")
        # Es inicio real solo si la ventana contiene etiquetas
        es_inicio[i] <- stringr::str_detect(ventana, PATRON_ETIQUETAS)
      }
    }
    
    df_segmentado <- temp_data %>%
      dplyr::mutate(
        es_inicio = es_inicio,
        #   suma el offset global para que la numeración sea continua ──
        id_paciente_unico = cumsum(es_inicio) + contador_global,
        #  
        indice_str = sprintf("%05d", id_paciente_unico),
        nombre_archivo = paste0(indice_str, ".txt")
      )
    
    num_pacientes <- length(unique(df_segmentado$nombre_archivo))
    message(paste("Archivo:", nombre_origen, "| Pacientes detectados:", num_pacientes))
    
    df_segmentado %>%
      dplyr::group_split(nombre_archivo) %>%
      purrr::walk(~ readr::write_lines(.x$X1, file.path(path_pacientes, unique(.x$nombre_archivo))))
    
    archivos_generados <- unique(df_segmentado$nombre_archivo)
    mapa_origen <- dplyr::bind_rows(mapa_origen,
      tibble::tibble(archivo = archivos_generados, origen = nombre_origen))
    
    # actualizar el contador con los pacientes de esta iteración ──
    contador_global <- contador_global + sum(es_inicio)
    #  
    
    message(paste("Completado:", nombre_origen))
  }
  
  # Guardar el mapa como CSV en path_pacientes
  readr::write_csv(mapa_origen, file.path(path_pacientes, "_mapa_origen.csv"))
  message(paste("Mapa de origen guardado en:", file.path(path_pacientes, "_mapa_origen.csv")))
  
  return(paste("Proceso finalizado. Archivos guardados en:", path_pacientes))
}

# 3 -------------------------------------------------------------------
# herramienta

# --- DNI HC FICM # sera usada dentro de fx evaluar pacientes    sacamos  FI
extraer_dato_clinico <- function(texto, etiqueta) {
  patron <- paste0("(?i)", etiqueta, "[:\\s]*([\\d/\\.]+)")
  match <- stringr::str_match(texto, patron)
  if (!is.na(match[1, 2])) return(stringr::str_trim(match[1, 2])) else return(NA_character_)
}

# 4 ----------- excluyendo duplicados

evaluar_pacientes <- function(path_pacientes) {
  
  # cargar mapa de origen 
  mapa_origen <- readr::read_csv(
    file.path(path_pacientes, "_mapa_origen.csv"),
    show_col_types = FALSE
  )
  
  pacientes_files <- list.files(path = path_pacientes, pattern = "\\.txt$", full.names = FALSE)
  # Add progress bar
  pb <- progress_bar$new(
    format = "  Evaluating [:bar] :current/:total (:percent) ETA: :eta",
    total = length(pacientes_files),
    clear = FALSE,
    width = 80
  )
  
  regex_exclusion <- PATTERN_EXCLUSION
  registro_inicial <- list() 
  
  for (p in pacientes_files) {
    pb$tick()  # Update progress
    ruta_origen <- file.path(path_pacientes, p)
    
    contenido <- read_lines(ruta_origen)
    if (length(contenido) < 2) next 
    
    # --- Extracción de Identidad ---
    linea_1 <- contenido[1]
    
    n_cama_v <-  linea_1 %>% str_extract("^\\d{4}") # str_extract("^\\d{4}\\s*[-]?\\s*")
    
    # 2. Remove bed number from line to avoid confusion
    linea_sin_cama <- linea_1 %>% str_remove("^\\d{4}\\s*-?\\s*")
    
    # 3. Extract age pattern from cleaned line
    edad_raw <- linea_sin_cama %>%
      str_extract(PATTERN_EDAD_COMPLETO)
    
    edad_v <- if (!is.na(edad_raw)) {
      as.integer(str_extract(edad_raw, "\\d{1,3}"))} else {NA_integer_}
    
    nombre_v <- linea_sin_cama %>%
      str_extract(PATTERN_NOMBRE_STOP_COMPLETO) %>%
      str_remove_all("[-,]") %>%
      str_remove("(?i)\\s*epicrisis\\s*\\.?\\s*docx?$") %>%
      str_remove("(?i)\\.docx?$") %>%
      str_trim() %>%
      str_squish() %>%
      toupper()
    
    # --- Datos Clínicos (Líneas 1-5) ---
    bloque_clinico <- paste(contenido[1:min(5, length(contenido))], collapse = " ")
    dni_v  <- extraer_dato_clinico(bloque_clinico, "DNI")
    hc_v   <- extraer_dato_clinico(bloque_clinico, "HC")
    ficm_v <- extraer_dato_clinico(bloque_clinico, "FICM")
    
    # --- Exclusiones y Laboratorio ---
    texto_completo <- tolower(paste(contenido, collapse = " "))
    hallazgo_exclusion <- str_extract(texto_completo, regex_exclusion)
    
    hallazgo_normalizado <- case_when(
      str_detect(hallazgo_exclusion, regex(PATTERN_EMBARAZO,  ignore_case = TRUE)) ~ "embarazo",
      str_detect(hallazgo_exclusion, regex(PATTERN_GESTACION, ignore_case = TRUE)) ~ "embarazo",
      str_detect(hallazgo_exclusion, regex(PATTERN_HEMORRAGIA,ignore_case = TRUE)) ~ "hemorragia",
      str_detect(hallazgo_exclusion, regex(PATTERN_POLITRAUMA,ignore_case = TRUE)) ~ "traumatismo",
      str_detect(hallazgo_exclusion, regex(PATTERN_TRAUMA,    ignore_case = TRUE)) ~ "traumatismo",
      TRUE ~ NA_character_
    )

    
    # --- Clasificación ---
    comentario_v <- case_when(
      !is.na(edad_v) & edad_v < 18                ~ "< 18 años",
      !is.na(hallazgo_normalizado)               ~ hallazgo_normalizado,
      TRUE                                        ~ "sigue"
    )
    
    # Almacenar en lista
    registro_inicial[[p]] <- tibble(
      archivo = p,
      cama = n_cama_v,
      nombre = nombre_v,    # cuando publiquemos hay que borrar esta variable de nobmre
      edad = edad_v, 
      dni = dni_v, 
      nro_hc = hc_v, 
      fi_clinica_medica = ficm_v,
      comentario = comentario_v
    )
    } #Cierre del bucle for
  
    # 2. Consolidar y aplicar UNIQUE (distinct)
    df_final <- bind_rows(registro_inicial) %>%
   
     # incorporar origen y posicionarlo segundo
    dplyr::left_join(mapa_origen, by = "archivo") %>%
    dplyr::relocate(origen, .after = archivo) %>%
    
    # 21 abril: investigadora indica no deduplicar, que traiga todo
    #distinct(nombre, dni, f_internacion, fi_clinica_medica, hb_inicial, .keep_all = TRUE) %>%
    
    # Reemplazo de comas por puntos en columnas de texto
    mutate(across(where(is.character), ~ str_replace_all(., ",", "."))) %>%
    # Aseguramos que hb_inicial sea numérica
    mutate(#hb_inicial = as.numeric(hb_inicial),
           edad = as.integer(edad))
  
  ### agrega validaciones
  df_final <- df_final %>%
    rowwise() %>%
    mutate(validacion_paciente = list(validar_paciente(cur_data()))) %>%
    unnest_wider(validacion_paciente)
  
  # Add validation summary to report
  validation_summary <- df_final %>%
    summarise(
      total_pacientes = n(),
      con_issues = sum(!validacion_edad_dni),
      edad_invalida = sum(str_detect(validacion_issues, "Edad")),
      dni_invalido = sum(str_detect(validacion_issues, "DNI"))
    )
  #### 
  
  return(df_final)
}

# 5 --------------------------------------------------------------------------

  # 3. Lógica de reclasificación vectorizada

clasificar <- function(tabla) {
  tabla_procesada <- tabla %>%
    dplyr::mutate(decision = dplyr::if_else(comentario != "sigue","EXCLUIR", "CONTINUAR")) %>%   # asi los vacios continuan tambien
    dplyr::relocate(decision, .after = dplyr::last_col())

  return(tabla_procesada)  ## esta es la que se analizara en analisis.R y formara parte del reporte final
}

# 6 --------------------------------------------------------------------------

organizar_archivos <- function(tabla, path_pacientes, path_excluidos) {
  # Filtrar solo los que deben ser excluidos
  archivos_a_excluir <- tabla %>% 
    dplyr::filter(decision == "EXCLUIR") %>% 
    dplyr::pull(archivo)
  
  if (length(archivos_a_excluir) == 0) {
    message("No hay archivos nuevos para excluir.")
    return(invisible(NULL))
  }
  
  for (f in archivos_a_excluir) {
    ruta_origen <- file.path(path_pacientes, f)
    ruta_destino <- file.path(path_excluidos, f)
    
    if (file.exists(ruta_origen)) {
      success <- file.rename(ruta_origen, ruta_destino)
      if (success) {
        message(paste("Movido a Excluidos:", f))
      } else {
        warning(paste("No se pudo mover el archivo:", f))
      }
    }
  }
}

