# library(dplyr)
# library(stringr)
# library(biostats)
# 

# source("funciones.R") # para obtener la tabla procesada

# o meterlo todo en una funcion 
analizando_resultados <- function(tabla) {
  
  if ("comentario_2" %in% colnames(tabla)) {
      tabla <- tabla %>%
      rename(comentario_previo = comentario) %>%           # cambio el nombre del viejo
      rename(comentario = comentario_2)  # Renombra el nuevo para que sea el principal
    
    cat("Actualización: Se ha reemplazado 'comentario' por 'comentario_2'.\n")
    
  } else {
    cat("Nota: 'comentario_2' no detectado, se mantiene el comentario original.\n")
  }
  
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
  
 
  # D. Grupos de exclusión
  grupos_excluidos <- tabla %>% 
    filter(decision == "EXCLUIR") %>% 
    #mutate(comentario_normalizado = str_remove(comentario, ",.*$")) %>%
    group_by(comentario) %>% 
    count() %>% 
    ungroup() %>% 
    janitor::adorn_totals(where = "row", name = "TOTAL EXCLUIDOS")
  
  # E. Función interna para métricas de HB, en todas los grupos voy a queres que no esten las hb vacias  y qeu haga el summarise con todas esas metricas
  metricas_hb <- function(tabla) {
    tabla %>% filter(!is.na(hb_inicial)) %>% 
      summarise(across(hb_inicial, list(min=min, max=max, media=mean, mediana=median, sd=sd), .names = "{.fn}"))
  }
  
  # F. Aplicar métricas por grupos
  hb_tot <- metricas_hb(tabla)
  hb_anemia <- metricas_hb(tabla %>% filter(decision == "EXCLUIR" & str_detect(comentario, "anemia")))
  hb_sin_anemia <- metricas_hb(tabla %>% filter(decision == "CONTINUAR"))
  

  # C. Tabla resumida para biostats
    resumir_tabla <- tabla %>% 
    #mutate(comentario_normalizado = str_remove(comentario, ",.*$")) %>% 
    select(-c(archivo, cama, nombre, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, comentario)) %>% 
    mutate(edad = as.numeric(edad),
           hb_inicial = as.numeric(hb_inicial))
    
  
  # G. Biostats
  bio_1 <- as.data.frame(biostats::summary_table(resumir_tabla, all = TRUE)) %>% select(-any_of("normality"))
  bio_2 <- as.data.frame(biostats::summary_table(resumir_tabla, group_by = "decision", all = TRUE, exclude = "sexo"))
  
  # devolver todo en una lista organizada
  return(list(
    counts = resumen_counts,
    vacios = vacios,
    excluidos = grupos_excluidos,
    hb_tot = hb_tot,
    hb_anemia = hb_anemia,
    hb_sin_anemia = hb_sin_anemia,
    bio1 = bio_1,
    bio2 = bio_2
  ))
  } # cierro la funcion


generar_reporte_calidad <- function(tabla, extraction_log) {
  
  # Extraction success rates
  extraccion_stats <- tibble(
    campo = c("cama", "nombre", "edad", "dni", "hb_inicial", "nro_hc", 
              "f_internacion", "fi_clinica_medica"),
    total = nrow(tabla),
    extraidos = c(
      sum(!is.na(tabla$cama)),
      sum(!is.na(tabla$nombre)),
      sum(!is.na(tabla$edad)),
      sum(!is.na(tabla$dni)),
      sum(!is.na(tabla$hb_inicial)),
      sum(!is.na(tabla$nro_hc)),
      sum(!is.na(tabla$f_internacion)),
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
      hb_valida = sum(hb_inicial >= 3 & hb_inicial <= 20, na.rm = TRUE),
      dni_valido = sum(str_detect(dni, "^\\d{7,8}$"), na.rm = TRUE)
    )
  
  # Duplicates analysis
  duplicates_stats <- tabla %>%
    group_by(nombre, dni) %>%
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

# Add to report
quality_report <- generar_reporte_calidad(tabla_final, extraction_log)

# Add sheet to Excel
hojas$Calidad_Datos <- quality_report$extraccion
hojas$Duplicados <- quality_report$duplicados