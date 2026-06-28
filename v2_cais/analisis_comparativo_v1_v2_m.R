


library(here) # set directory
library(readtext)
library(readr)
library(readxl)
library(tidyverse)
library(stringi)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate) # por si despues usamos fechas
library(janitor) # adorn
library(tibble) # para reporte final
library(writexl) # para hacer el excel final
library(googlesheets4)
library(progress)  # CLAUD
library(logger)  # CLAUD
library(testthat)
library(DT)
library(fuzzyjoin)  ## no use al final
library(stringdist)
library(irr) # para kappa2

carpeta = "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/junio/comparacionV1V2M/"

# --- 1
# Traigo los 3 df : y les hago el distinct, 
# agrupando los nombres de archivo en V1 y V2 en caso de registros totalmente duplicados
# v1 y v2 tienen todas las col: archivo, origen, etc, etc
# manual no tiene archivo
# hb inicial as character para que no le meta ceros q no van 

# no considero la columna Sexo ya que en los V1 y v2 solo lo pido para los que se requiera

#--- 1.1 3 df enteros
vv1 <- read_xlsx(paste0(carpeta,"ReporteV1.xlsx"), sheet="Tabla_Pacientes" ) %>%
  select(origen, archivo, cama, nombre, edad, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, hb_inicial, comentario, decision) %>% 
  mutate(archivo = str_remove(archivo, "\\.txt$"),
         origen = str_remove(origen, "\\.txt$"),
         hb_inicial = as.character(hb_inicial)) 
vv1n <- nrow(vv1)
v1 <- vv1 %>% 
  group_by(origen, cama, nombre, edad, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, hb_inicial, comentario, decision) %>% 
  mutate(archivo = paste(archivo, collapse=" - ")) %>% 
  distinct() %>% 
  ungroup()


vv2 <-  read_xlsx(paste0(carpeta,"ReporteV2.xlsx"), sheet="Tabla_Pacientes" ) %>% 
  select(archivo,origen, cama, nombre, edad, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, hb_inicial, comentario, decision) %>% 
  mutate(archivo = str_remove(archivo, "\\.txt$"),
         origen = str_remove(origen, "\\.txt$"),
         hb_inicial = as.character(hb_inicial)) 
vv2n <- nrow(vv2)
v2 <- vv2 %>% 
  group_by(origen, cama, nombre, edad, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, hb_inicial, comentario, decision) %>% 
  mutate(archivo = paste(archivo, collapse=" - ")) %>% 
  distinct() %>% 
  ungroup()


m_manual <- read_xlsx(paste0(carpeta,"M_limpia.xlsx")) %>%
  select(origen, cama, nombre, edad, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, hb_inicial,comentario, decision ) %>% 
  mutate(origen = str_remove(origen, "\\.txt$"),
         edad = as.numeric(edad))
m_manual_n <- nrow(m_manual)
manual <- m_manual %>% 
  distinct() # como no tiene nro de archivo no hay problema que requiera colapsar 


nrow(v2) #434
table(v2$decision) # C133 E301

nrow(manual) #435
table(manual$decision) # C180 E255

rm(vv1,vv2,m_manual)

# --- 2
# definicion de funcion fuzzy union: 
# ¿que es lo que hacemos? vamos a hacer que en caso de registros que son iguales en v1 y en manual, se le asigna 
# al manual el nro de archivo igual al de el del pipe (sea v1 o v2).
# casos donde el registro no es enteramente igual, el join fuzzy busca el mas similar por 

asignar_archivo_fuzzy <- function(manual, pipeline, n = 3) {
  
  opciones_cols <- list(
    `1` = c("origen", "cama", "edad", "dni", "nro_hc", 
            "f_internacion", "fi_clinica_medica", "f_hb_inicial", "hb_inicial", "comentario", "decision"),
    `2` = c("origen", "cama", "edad", "dni", "nro_hc", 
            "f_internacion", "fi_clinica_medica", "f_hb_inicial", "hb_inicial", "comentario"),
    `3` = c("origen", "cama", "edad", "dni", "nro_hc", 
            "f_internacion", "fi_clinica_medica", "f_hb_inicial", "hb_inicial")
  )
  
  cols_exactas <- opciones_cols[[as.character(n)]]
  
  if (is.null(cols_exactas)) stop("n debe ser 1, 2 o 3")
  
  cat("Usando configuracion", n, "| Columnas consideradas para match:", length(cols_exactas), "\n")
  
  # ETAPA 1: join exacto sin tocar la columna 'nombre'
  # Solo traemos 'archivo' de pipeline. 'nombre' se queda intacto en 'manual'.
  con_match <- manual %>%
    left_join(pipeline %>% select(archivo, all_of(cols_exactas)),
              by = cols_exactas)
  
  # Separamos los que no hicieron match exacto
  sin_match <- con_match %>% 
    filter(is.na(archivo)) %>% 
    select(-archivo) 
  
  con_match <- con_match %>% 
    filter(!is.na(archivo))
  
  cat("Exactos:", nrow(con_match), "| Sin match:", nrow(sin_match), "\n")
  
  ## ETAPA 2: fuzzy por nombre + filtro exacto por origen y cama
  # hacer el fuzzy solo por nombre pero luego filtrar por campos exactos adicionales para desambiguar:
  
  nombres_pipeline  <- pipeline$nombre
  archivos_pipeline <- pipeline$archivo
  
  fuzzy <- sin_match %>%
    mutate(
      idx_min  = purrr::map_int(nombre, ~ which.min(stringdist::stringdist(.x, nombres_pipeline, method = "lv"))),
      dist_min = purrr::map_dbl(nombre, ~ min(stringdist::stringdist(.x, nombres_pipeline, method = "lv")))
    ) %>%
    filter(dist_min <= 2) %>%
    mutate(
      archivo          = archivos_pipeline[idx_min],
      origen_candidato = pipeline$origen[idx_min],
      cama_candidato   = pipeline$cama[idx_min],
      dni_candidato    = pipeline$dni[idx_min]
    ) %>%
    # validar que el candidato tenga sentido clínico
    filter(
      origen == origen_candidato,
      cama   == cama_candidato | is.na(cama) | is.na(cama_candidato),
      dni    == dni_candidato  | is.na(dni)  | is.na(dni_candidato)
    ) %>%
    select(-idx_min, -dist_min, -origen_candidato, -cama_candidato, -dni_candidato)
  
  sin_match_final <- sin_match %>%
    filter(!nombre %in% fuzzy$nombre)
  
  cat("Fuzzy matches:", nrow(fuzzy), "| Sin match final:", nrow(sin_match_final), "\n")
  
  # Unimos todo de vuelta
  bind_rows(con_match, fuzzy, sin_match_final) %>%
    relocate(origen, archivo, .before = 1)
}



# 
# El dataset de entrada con todas las columnas; la función solo usa las que están en cols_exactas para el join.
# El efecto es en cuántos registros logran match exacto en la Etapa 1, 
# lo que determina cuántos pasan al fuzzy y cuántos quedan sin par:
#   n=1 (más columnas) → el join exige que todas coincidan incluyendo comentario y decision → 
#       menos matches exactos → más registros van al fuzzy → las métricas de concordancia van a ser 
#       más bajas porque más pares son imperfectos
#   n=3 (menos columnas) → el join es más permisivo → más matches exactos → métricas más altas, 
#       pero pueden incluir pares que coincidieron en todo menos en comentario/decision, 
#       que es precisamente lo que querés medir
# 
# Luego en la comparación celda a celda se incluyen todas las cols, incluso comentario y decision en cols_comparar, 
# para que esas columnas también aparezcan en el resumen de concordancia
# 
# Así separás el problema de identificación (qué par de registros comparar) del problema de medición (qué tan distintos son esos pares).
# 
# 

# USO FUNCION ASIGNAR NRO ARCHIVO AL MANUAL COINCIDENCIA EXACTA Y FUZZY 
# con parametro...  n=1 con todas las columnas;  
#                   n=2 sin columna decision; 
#                   n=3 sin columnas decision ni comentario; 

manual_v1_3 <- asignar_archivo_fuzzy(manual, v1, n = 3)
manual_v1_2 <- asignar_archivo_fuzzy(manual, v1, n = 2)
manual_v1_1 <- asignar_archivo_fuzzy(manual, v1, n = 1)

manual_v2_3 <- asignar_archivo_fuzzy(manual, v2, n = 3)
manual_v2_2 <- asignar_archivo_fuzzy(manual, v2, n = 2)
manual_v2_1 <- asignar_archivo_fuzzy(manual, v2, n = 1)


# --- Funcion para comparar datasts (Deriva de lo que paso paula)

comparar_y_exportar <- function(M, P, nombre_M, nombre_P, carpeta, m0 = m_manual_n, v0) {
  
  # ---- 3. Detectar historias sin match -----------------------
  solo_en_P <- P %>% filter(!archivo %in% M$archivo)
  solo_en_M <- M %>% filter(!archivo %in% P$archivo)
  cat("Registros en P sin match en M:", nrow(solo_en_P), "\n")
  cat("Registros en M sin match en P:", nrow(solo_en_M), "\n")
  
  # ---- 4. Unir por archivo ------------------------------------
  cols_comunes  <- intersect(names(P), names(M))
  cols_comparar <- setdiff(cols_comunes, c("archivo", "id_norm"))
  
  comparacion <- left_join(
    M %>% select(archivo, all_of(cols_comparar)),
    P %>% select(archivo, all_of(cols_comparar)),
    by = "archivo",
    suffix = c("_M", "_P")
  )
  
  # ---- 5. Comparar columna por columna ------------------------
  comparar_valores <- function(p, m) {
    case_when(
      is.na(p) & is.na(m) ~ "Igual",
      is.na(p) | is.na(m) ~ "Diferente",
      str_trim(str_to_upper(as.character(p))) ==
        str_trim(str_to_upper(as.character(m))) ~ "Igual",
      TRUE ~ "Diferente"
    )
  }
  
  for (col in cols_comparar) {
    comparacion[[paste0(col, "_match")]] <- comparar_valores(
      comparacion[[paste0(col, "_M")]],
      comparacion[[paste0(col, "_P")]]
    )
  }
  
  comparacion <- comparacion %>% arrange(nombre_P)
  
  # ---- 6. Resumen ---------------------------------------------
  resumen <- tibble(
    columna      = cols_comparar,
    n_total      = nrow(comparacion),
    n_igual      = sapply(cols_comparar, function(col) {
      sum(comparacion[[paste0(col, "_match")]] == "Igual")}),
    total_M      = nrow(M),
    con_par_en_P = nrow(M) - nrow(solo_en_M),
    sin_par_en_P = nrow(solo_en_M),
    pct_cobertura = round(100 * (nrow(M) - nrow(solo_en_M)) / nrow(M), 1)
  ) %>%
    mutate(pct_concordancia = round(100 * n_igual / n_total, 1))
  
  print(resumen)
  
  # ---- 7. Exportar --------------------------------------------
  comparacion_vista <- comparacion %>% select(archivo, contains("match"))
  
  hoja_resumen <- tribble(
    ~"", ~"",
    "n registros en Manual original =", paste(m0),
    paste("n registros en ", nombre_P, " original ="), paste(v0),    
    "n registros en Manual (desduplicados) =",        paste(nrow(M)),
    paste0("n registros en ", nombre_P, " (desduplicados) ="), paste(nrow(P)),
    paste0("n archivos en ", nombre_P, " sin match en Manual:"), paste(nrow(solo_en_P)),
    paste0("n archivos en Manual sin match en ", nombre_P, ":"), paste(nrow(solo_en_M)),
    "", "", "", "", "", ""
  )
  
  nombre_archivo <- paste0("reporte_", nombre_M, "_vs_", nombre_P, ".xlsx")
  
  reporte <- list(
    "numeros"     = hoja_resumen,
    "comparacion" = comparacion_vista,
    "resumen"     = resumen
  )
  
  write_xlsx(reporte, file.path(carpeta, nombre_archivo))
  cat("Exportado:", nombre_archivo, "\n\n")
  
  invisible(list(comparacion = comparacion, 
                 resumen = resumen))

  
    if (nombre_M == "manual_v2_3") {
    saveRDS(comparacion, file.path(carpeta, "comparacion_manual_v2_3_completo.rds"))
  }
}


# siendo que con todas las columnas es igual a la de sin la de decision, vamos a simplificar en 
# hacer las comparaciones con todas las columnas vs sin las ultimas dos columnas 

comparar_y_exportar(manual_v1_1, v1, "manual_v1_1", "v1", carpeta,, vv1n)
comparar_y_exportar(manual_v1_3, v1, "manual_v1_3", "v1", carpeta,, vv1n)

comparar_y_exportar(manual_v2_1, v2, "manual_v2_1", "v2", carpeta,, vv2n)
comparar_y_exportar(manual_v2_3, v2, "manual_v2_3", "v2", carpeta,, vv2n)



# --- AHORA QUIERO CALCULAR EL VPP QUE DIJIMOS QUE IBAMOS A CALCULAR EN EL PAPER
# SERA de V2 vs manual 

comparacion_manual_v2_3_completo <-readRDS(file.path(carpeta, "comparacion_manual_v2_3_completo.rds")) 

v2_m <- readRDS(file.path(carpeta, "comparacion_manual_v2_3_completo.rds")) %>%
  select(archivo, decision_M, decision_P)


# Los FP (3) los tenés de solo_en_P del análisis previo

# Continuar en v2 y en manual
VP <- v2_m %>% 
  filter (decision_M == "CONTINUAR",
          decision_P == "CONTINUAR") %>% 
  n_distinct("archivo")

# excluir en V2, continuar en manual
FN <- v2_m %>% 
  filter (decision_M == "CONTINUAR",
          decision_P == "EXCLUIR") %>% 
  n_distinct("archivo")

FP <- v2_m %>% 
  filter (decision_M == "EXCLUIR",
          decision_P == "CONTINUAR") %>% 
  n_distinct("archivo")

VN <- v2_m %>% 
  filter(decision_P == "EXCLUIR",
         decision_M == "EXCLUIR") %>% 
  n_distinct("archivo")


VPP = VP/(VP+FP)

VPN = VN/(FP+VN)

S = VP/(VP+FN)

E = VN /(VN+FP)


cat(sprintf(
  "Resultados del Tamizaje (V2 vs Manual):\n
  Sensibilidad (S):          %.2f%%\n
  Especificidad (E):         %.2f%%\n
  Valor Predictivo Pos (VPP): %.2f%%\n
  Valor Predictivo Neg (VPN): %.2f%%\n", 
  S*100, E*100, VPP*100, VPN*100
))


table("Pipeline R V2" = v2_m$decision_P, "Anotacion Manual, Gold Standard" = v2_m$decision_M)

knitr::kable(data.frame(
  Métrica = c("Sensibilidad", "Especificidad", "Valor Predictivo Positivo (VPP)", "Valor Predictivo Negativo (VPN)"),
  Valor = sprintf("%.2f%%", c(S*100, E*100, VPP*100, VPN*100))
))

# Crear una estructura de matriz con tus resultados
matriz_contingencia <- matrix(
  c(VP, FP, FN, VN), 
  nrow = 2, 
  byrow = TRUE,
  dimnames = list(PipelineR_V2 = c("CONTINUAR", "EXCLUIR"),
                  Anotacion_Manual = c("CONTINUAR", "EXCLUIR"))
)

# Graficar
print(matriz_contingencia, color = c("#cc6666", "#99cc99"), conf.level = 0, margin = 1)





# --- COEFICIENTE KAPPA

# -- El coeficiente Kappa de Cohen mide la concordancia entre dos clasificadores (manual vs V2) más allá del azar.
# LOS CLASIFICADORES asignan cada paciente a una de dos categorías (CONTINUAR / EXCLUIR).



kappa_data <- comparacion_manual_v2_3_completo %>%
  select(decision_M, decision_P) %>% 
  filter(!is.na(decision_M), !is.na(decision_P)) %>%
  select(decision_M, decision_P)
# 431 registros porque hay 4 de v2 que no matchearon con manual (1 si que no esta en v2 y 3 que no matchearon )

#install.packages("irr")
resultado_kappa <- irr::kappa2(kappa_data, weight = "unweighted")
print(resultado_kappa)

table(comparacion_manual_v2_3_completo$decision_P,comparacion_manual_v2_3_completo$decision_M, 
      dnn = c("V2","Manual" )) %>% 
  addmargins()



source("~/R Studio/PaulaAnemias/deprecado/ConClaude_v2/generar_reporte.R")

source("~/R Studio/PaulaAnemias/deprecado/ConClaude_v2/analisis_resultados.R")

analizando_resultados(comp)
