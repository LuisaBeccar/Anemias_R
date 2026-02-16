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

# 2 ----------------------------------------------------------
segmentar_pacientes <- function(path_proyecto, path_pacientes, path_excluidos) {
  
  # 1. Crear carpetas de destino si no existen
  if (!dir.exists(path_pacientes)) dir.create(path_pacientes, recursive = TRUE)
  if (!dir.exists(path_excluidos)) dir.create(path_excluidos, recursive = TRUE)
  
  # 2. Listar archivos .txt
  archivos_pendrive <- list.files(path = path_proyecto, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(archivos_pendrive) == 0) {
    stop("No se encontraron archivos .txt en la ruta especificada.")
  }
  
  # 3. Procesar cada archivo
  for (archivo_txt in archivos_pendrive) {
    # Leer el archivo. Usamos read_delim del paquete readr
    temp_data <- readr::read_delim(archivo_txt, delim = "\n", col_names = "X1",
                                   show_col_types = FALSE, quote = "")

    # Segmentación y limpieza
    df_segmentado <- temp_data %>%
      dplyr::mutate(
        # Identificar inicio de registro de paciente
        es_inicio = stringr::str_detect(X1, "^\\d{4}\\s*-?\\s*[a-zA-ZáéíóúÁÉÍÓÚÑñ\\s]+\\s*(\\(\\d+\\)|\\d{1,2})?"),
        id_paciente_unico = cumsum(es_inicio) # 2. ID secuencial único: Cada paciente tendrá su propio número del 1 al n
      ) %>%
      #dplyr::filter(id_paciente_unico > 0) %>%
      #dplyr::group_by(id_paciente_unico) %>%
      dplyr::mutate(
        #id_4digitos = stringr::str_extract(dplyr::first(X1), "^\\d{4}"),
        indice_str = sprintf("%05d", id_paciente_unico),
        # clave_aleatoria = stri_rand_strings(1, 5),  ## quizas no es necesario.. 
        nombre_archivo = paste0(indice_str,  ".txt") # opcional "_", id_4digitos,"_", clave_aleatoria,
      )# %>%
      #dplyr::ungroup()
    
    # Verificación en consola
    num_pacientes <- length(unique(df_segmentado$nombre_archivo))
    message(paste("Archivo:", basename(archivo_txt), "| Pacientes detectados:", num_pacientes))
    
    # 4. Guardar cada paciente por separado
    df_segmentado %>%
      dplyr::group_split(nombre_archivo) %>%
      purrr::walk(~ readr::write_lines(.x$X1, file.path(path_pacientes, unique(.x$nombre_archivo))))
    message(paste("Completado:", basename(archivo_txt)))
  }
  return(paste("Proceso finalizado. Archivos guardados en:", path_pacientes))
}


# 3 -------------------------------------------------------------------

# herramientas

# --- DNI HC FI FICM # sera usada dentro de fx evaluar pacientes 
extraer_dato_clinico <- function(texto, etiqueta) {
  patron <- paste0("(?i)", etiqueta, "[:\\s]*([\\d/\\.]+)")
  match <- stringr::str_match(texto, patron)
  if (!is.na(match[1, 2])) return(stringr::str_trim(match[1, 2])) else return(NA_character_)
}


# --- Hemoglobina inicial  # sera usada dentro de fx evaluar pacientes 
extraer_hb_inicial <- function(contenido, idx_lab) {
  # 1. Validación de seguridad inicial
  if (is.na(idx_lab) || idx_lab >= length(contenido)) {
    return(list(valor = NA_real_, fecha = NA_character_))
  }
  
  # 2. Definir ventana de búsqueda (10 líneas)
  limite_superior <- min(idx_lab + 10, length(contenido))
  rango_busqueda <- (idx_lab + 1):limite_superior
  
  for (i in rango_busqueda) {
    linea_lab <- contenido[i]
    
    # 3. Detectar línea con formato Fecha: Datos
    if (stringr::str_detect(linea_lab, "^\\d{1,2}/\\d{1,2}:")) {
      
      # Extraer la fecha
      fecha_detectada <- stringr::str_extract(linea_lab, "^\\d{1,2}/\\d{1,2}")
      
      # Limpiar la línea y dividir por "/"
      datos_puros <- stringr::str_remove(linea_lab, "^\\d{1,2}/\\d{1,2}:") %>% 
        stringr::str_trim()
      
      partes <- stringr::str_split_1(datos_puros, "/")
      
      # 4. Evaluar la segunda posición (índice 2)
      if (length(partes) >= 2) {
        # Extraer solo el número (soporta decimales)
        valor_candidato <- as.numeric(stringr::str_extract(partes[2], "\\d+\\.?\\d*"))
        
        # Validación de rango clínico (Hemoglobina válida entre 3 y 45)
        if (!is.na(valor_candidato) && valor_candidato >= 3.0 && valor_candidato <= 45.0) {
          return(list(valor = valor_candidato, fecha = fecha_detectada))
        }
      }
    }
    
    # 5. Parada de emergencia: si hay línea vacía o nueva sección con "Texto:"
    if (linea_lab == "" || stringr::str_detect(linea_lab, "^[A-Z][a-z]+:")) {
      break
    }
  }
  
  # Si recorre todo el rango y no encuentra nada, devuelve NA
  return(list(valor = NA_real_, fecha = NA_character_))
}


# 4 ----------- excluyendo duplicados    BORRAR TODO LO DE EPICRISIS QUE NO VAMOS A USAR

evaluar_pacientes <- function(path_pacientes, path_excluidos) {
  
  # 1. Configurar rutas de exclusión
  if (!dir.exists(path_excluidos)) dir.create(path_excluidos)
  #path_epicrisis <- file.path(path_excluidos, "Epicrisis")
  #if (!dir.exists(path_epicrisis)) dir.create(path_epicrisis, recursive = TRUE)
  
  pacientes_files <- list.files(path = path_pacientes, pattern = "\\.txt$", full.names = FALSE)
  regex_exclusion <- "((?<!tuvo |niega |sin |de )embaraz(?!os|.*(negativo|-))|gestac(?!.*(negativo|-))|hemorrag|politrauma|trauma(?!to|log|tolo))"
  registro_inicial <- list() 
  #conteo_epicrisis <- 0
  
  for (p in pacientes_files) {
    ruta_origen <- file.path(path_pacientes, p)
    
    
    ## resto de analisis si no dice epicrisis
    contenido <- read_lines(ruta_origen)
    if (length(contenido) < 2) next 
    
    # --- Extracción de Identidad ---
    linea_1 <- contenido[1]
    
    n_cama_v <-  linea_1 %>% str_extract("^\\d{4}\\s*[-]?\\s*")
    
    edad_raw <- linea_1 %>% 
      str_extract("^\\d{4}\\s*[-]?\\s*") %>%
      str_remove("\\s*[-]?\\s*.") %>% 
      str_extract("(?i)(\\(\\s*(edad\\s*)?\\d[\\s\\d]{1,3}(.*?)\\)|\\bedad[:\\s]*\\d[\\s\\d]{1,3}\\b|\\b\\d[\\s\\d]{1,3}\\s*(años?|ños|a|(?=\\s*-?DNI|\\s*-?HC|$)))")
    
    edad_v <- if (!is.na(edad_raw)) as.numeric(str_remove_all(edad_raw, "[^0-9]")) else NA_real_
    
    nombre_v <- linea_1 %>%
      str_replace("^\\d{4}\\s*[-]?\\s*", "") %>%
      str_extract("(?i)^.*?(?=\\s*\\(\\s*(edad|\\d+)|\\s+edad|\\s+\\d+|\\s*(DNI|HC|FI)|$)") %>% 
      str_remove_all("[-|,]") %>% 
      str_trim() %>% 
      toupper()
    
    # --- Datos Clínicos (Líneas 1-5) ---
    bloque_clinico <- paste(contenido[1:min(5, length(contenido))], collapse = " ")
    dni_v  <- extraer_dato_clinico(bloque_clinico, "DNI")
    hc_v   <- extraer_dato_clinico(bloque_clinico, "HC")
    fi_v   <- extraer_dato_clinico(bloque_clinico, "FI")
    ficm_v <- extraer_dato_clinico(bloque_clinico, "FICM")
    
    # --- Exclusiones y Laboratorio ---
    texto_completo <- tolower(paste(contenido, collapse = " "))
    hallazgo_exclusion <- str_extract(texto_completo, regex_exclusion)
    
    hallazgo_normalizado <- case_when(
      str_detect(hallazgo_exclusion, "embaraz|gestac") ~ "embarazo",
      str_detect(hallazgo_exclusion, "hemorrag")      ~ "hemorragia",
      str_detect(hallazgo_exclusion, "politrauma|trauma") ~ "traumatismo",
      TRUE ~ NA_character_
    )
    
    idx_lab <- which(str_detect(contenido, regex("laboratorios:", ignore_case = TRUE)))[1]
    res_hb <- extraer_hb_inicial(contenido, idx_lab)
    
    # --- Clasificación ---
    comentario_v <- case_when(
      !is.na(edad_v) & edad_v < 18                ~ "< 18 años",
      !is.na(hallazgo_normalizado)               ~ hallazgo_normalizado,
      !is.na(res_hb$valor) & res_hb$valor < 12.0  ~ "anemia basal",
      is.na(res_hb$valor)                         ~ "falta hb inicial",
      !is.na(res_hb$valor) & res_hb$valor >= 12.0 & res_hb$valor < 13.0 ~ "evaluar sexo",
      TRUE                                        ~ "sigue"
    )
    
    # --- Mover archivos excluidos (opcional) ---
    if (comentario_v %in% c("< 18 años", "embarazo", "hemorragia", "traumatismo")) {
      file.rename(ruta_origen, file.path(path_excluidos, p))
    }
    
    # Almacenar en lista
    registro_inicial[[p]] <- tibble(
      archivo = p,
      cama = n_cama_v,
      nombre = nombre_v,    # cuando publiquemos hay que borrar esta variable de nobmre
      edad = edad_v, 
      dni = dni_v, 
      nro_hc = hc_v, 
      f_internacion = fi_v, 
      fi_clinica_medica = ficm_v,
      f_hb_inicial = res_hb$fecha, 
      hb_inicial = res_hb$valor, 
      comentario = comentario_v
    )
    } #Cierre del bucle for
  
  # 2. Consolidar y aplicar UNIQUE (distinct)
  df_final <- bind_rows(registro_inicial) %>%
    distinct(nombre, dni, f_internacion, fi_clinica_medica, hb_inicial, .keep_all = TRUE) %>%
    
    # Reemplazo de comas por puntos en columnas de texto
    mutate(across(where(is.character), ~ str_replace_all(., ",", "."))) %>%
    # Aseguramos que hb_inicial sea numérica
    mutate(hb_inicial = as.numeric(hb_inicial))
  
  return(df_final)
  }

# 5 --------------------------------------------------------------------------
solicitar_sexo <- function(tabla) {
  # 1. Preparar columna sexo si no existe
  if (!"sexo" %in% names(tabla)) {
    tabla <- tabla %>% dplyr::mutate(sexo = NA_character_)
  }
  
  # 2. Bucle interactivo
  casos_evaluar <- which(tabla$comentario == "evaluar sexo")
  
  if (length(casos_evaluar) > 0) {
    message("\n>>> SE REQUIERE REVISIÓN MANUAL DE SEXO PARA ", length(casos_evaluar), " CASOS <<<\n")
    
    for (i in casos_evaluar) {
      mensaje <- paste0("¿Es ", tabla$nombre[i], " de sexo MASCULINO? (Y/N): ")
      respuesta <- readline(prompt = mensaje)
      
      if (toupper(respuesta) == "Y") {tabla$sexo[i] <- "M"
      } else if (toupper(respuesta) == "N") { tabla$sexo[i] <- "F"
      } else {
        message("Respuesta no válida para ", tabla$nombre[i], ". Se mantiene como NA.")
      }
    }
    cat("\n--------------------------------------------------------------\n")
    cat("Gracias por completar la revisión manual. Continuando análisis...")
    cat("\n--------------------------------------------------------------\n")
  }
  
  # 3. Lógica de reclasificación vectorizada
  tabla_procesada <- tabla %>% 
    dplyr::mutate(
      comentario = dplyr::case_when(
        sexo == "M" & comentario == "evaluar sexo" ~ "anemia basal",
        sexo == "F" & comentario == "evaluar sexo" ~ "sigue",
        TRUE                                       ~ comentario ),
      decision = dplyr::if_else(comentario != "sigue","EXCLUIR", "CONTINUAR")) %>%   # asi los vacios continuan tambien
    dplyr::relocate(sexo, comentario, .after = hb_inicial) %>% 
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

# 7 --------------------------------------------------------------------------

# despues de corregir o agregar sexos en google sheets, se volvio a cargar el csv y analizo denuevo anemias

revision_tabla <- function(tabla) {

  tabla_procesada <- tabla %>% 
    dplyr::mutate(
      # 1. Determinamos si TIENE anemia según el sexo recién completado en el csv sheets
      # en nueva columna que sera temporal
                  motivo_anemia = dplyr::case_when(
                    sexo == "F" & hb_inicial < 12 ~ "anemia basal",
                    sexo == "M" & hb_inicial < 13 ~ "anemia basal",
                    TRUE                          ~ "sigue" ),
      
      # 2. Lógica de pegado inteligente (comentario_3)
      # esta complicado pero creo q cumple
                  comentario_3 = dplyr::case_when(
                    # Caso A: Ya decía anemia basal y el nuevo cálculo confirma anemia basal -> No cambia
                    comentario == "evaluar sexo" & motivo_anemia == "anemia basal" ~ "anemia basal",
                    
                    # Caso C: Tenía otro motivo (ej: "< 18") y además es anemia -> Los pega
                    comentario != "sigue" & comentario != "evaluar sexo" & motivo_anemia == "anemia basal" & !str_detect(comentario, "anemia") ~ 
                      paste(comentario, "anemia basal", sep = " y "), # agrego que ademas de la causa clinica o edad tiene anemia basal
                    
                    # Caso D: En cualquier otro caso, mantenemos el comentario original
                    TRUE ~ comentario )
                  ) %>% 
    # 3. Limpieza y actualización de decisión
    dplyr::mutate(comentario_2 = comentario_3) %>% 
    dplyr::select(-motivo_anemia, -comentario_3) %>%  # queda comentario y comentario_2
    dplyr::mutate(decision = dplyr::if_else(comentario_2 == "sigue", "CONTINUAR", "EXCLUIR")) %>%   # rechequeo la decision
    # 4. Reorganización estética
    dplyr::relocate(sexo, .after = hb_inicial) %>% 
    dplyr::relocate(comentario, comentario_2, .after = sexo) %>% 
    dplyr::relocate(decision, .after = dplyr::last_col())
  
  return(tabla_procesada)
}
