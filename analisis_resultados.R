# library(dplyr)
# library(stringr)
# library(biostats)
# 

# source("funciones.R") # para obtener la tabla procesada

# o meterlo todo en una funcion 
analizando_resultados <- function(tabla) {
  
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
    mutate(comentario_normalizado = str_remove(comentario, ",.*$")) %>%
    group_by(comentario_normalizado) %>% 
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
    mutate(comentario_normalizado = str_remove(comentario, ",.*$")) %>% 
    select(-c(archivo, nombre, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, comentario))
  
  # G. Biostats
  bio_1 <- as.data.frame(biostats::summary_table(resumir_tabla)) %>% select(-any_of("normality"))
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