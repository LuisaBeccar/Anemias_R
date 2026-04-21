
analizando_resultados <- function(tabla) {
  
  tabla <- tabla 
  
  # A. Conteos básicos
  resumen_counts <- list(
    n_archivos = n_distinct(tabla$archivo),
    n_dni = n_distinct(tabla$nombre, tabla$dni),
    n_nombres = n_distinct(tabla$nombre),
    continuan = sum(tabla$decision == "CONTINUAR"),
    excluidos = sum(tabla$decision == "EXCLUIR")
  )
  
  # B. Tabla de vacíos
  vacios <- data.frame(
    columna = colnames(tabla), 
    cantidad_vacios = colSums(is.na(tabla)), 
    row.names = NULL
  )
 
  # C. Grupos de exclusión
  grupos_excluidos <- tabla %>% 
    filter(decision == "EXCLUIR") %>% 
    group_by(comentario) %>% 
    count() %>% 
    ungroup() %>% 
    janitor::adorn_totals(where = "row", name = "TOTAL EXCLUIDOS")
 
  # D. Tabla resumida para biostats
    resumir_tabla <- tabla %>% 
    select(-c(archivo, cama, nombre, dni, nro_hc, fi_clinica_medica, comentario)) %>%  
    mutate(edad = as.numeric(edad))
   
  # devolver todo en una lista organizada
  return(list(
    counts = resumen_counts,
    vacios = vacios,
    excluidos = grupos_excluidos))
  } # cierro la funcion


generar_reporte_calidad <- function(tabla, extraction_log) {
  
  # Extraction success rates
  extraccion_stats <- tibble(
    campo = c("cama", "nombre", "edad", "dni",  "nro_hc", "fi_clinica_medica"), 
    total = nrow(tabla),
    extraidos = c(
      sum(!is.na(tabla$cama)),
      sum(!is.na(tabla$nombre)),
      sum(!is.na(tabla$edad)),
      sum(!is.na(tabla$dni)),
      sum(!is.na(tabla$nro_hc)),
      sum(!is.na(tabla$fi_clinica_medica))
    )
  ) %>%
    mutate(
      tasa_exito = round(extraidos / total * 100, 1),
      faltantes = total - extraidos
    )
  
  # Validation stats
  validation_stats <- tabla %>%
    summarise(
      total_pacientes = n(),
      edad_valida = sum(edad >= 18 & edad <= 120, na.rm = TRUE),
      dni_valido = sum(str_detect(dni, "^\\d{7,8}$"), na.rm = TRUE)
    )
  
  # Duplicates analysis
  duplicates_stats <- tabla %>%
    group_by(nombre, dni) %>%   # 
    filter(n() > 1) %>%
    summarise(
      n_duplicados = n(),
      archivos = paste(archivo, collapse = ", ")
    )
  
  return(list(
    extraccion = extraccion_stats,
    validacion = validation_stats,
    duplicados = duplicates_stats
  ))
}

