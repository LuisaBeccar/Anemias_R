
#por si acaso resteo wd
setwd("C:/Users/luisa/OneDrive/Documentos/R Studio/PaulaAnemias/deprecado/ConClaude_v2/")

# Source all modules
source("librerias.R")
source("config.R")           # Load config FIRST (has path definitions)
source("funciones.R")        # Must come after patterns.R
source("analisis_manual_fxs.R")
source("generar_reporte.R")

path_proyecto <- CONFIG$paths$proyecto
# 
# =================== aca traigo el csv de anotacion manual 
# =========== y  le hago data wrangling para poder despues compararlo con la tabla final del pipe 

manual <- read_delim("C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/junio/manual/AnotacionManual.csv", col_types = "ciciccccccfff____")
#
# # c = character
# # i = integer
# # n = number
# # d = double
# # l = logical
# # f = factor
# # D = date
# # T = date time
# # t = time
# # ? = guess
# # _ or - = skip
#

tabla_manual <- manual %>%
  # emprolijo nombres de columna
  clean_names() %>%
  # decision ultima col
  rename(decision = if_else_col_l) %>%
  # saco espacios en blanco antes o despues de texto que hayan quedado
  mutate(across(everything(), trimws)) %>%
  # col nombre, todo mayuscula y sacar comas que hubiera entre apellid, nombre
  mutate(nombre = toupper(nombre),
         nombre = str_replace_all(nombre, "[^[:alnum:][:space:]]", "")) %>%
  #columna de hb todo con punto decimal en vez de coma y pasar a numeric
  mutate(hb_inicial = str_replace_all(hb_inicial, ",", "."),
         #hb_inicial = as.numeric(hb_inicial)
         )
rm(manual)
message("--- Generando tabla_manual_limpia ---")

ruta_archivo_m = ("C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/junio/manual/tabla_manual_limpia.xlsx")
#otra copia para comparaciones
ruta_archivo_m2 = ("C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/junio/comparacionV1V2M/M_limpia.xlsx")

write_xlsx(tabla_manual, ruta_archivo_m)
write_xlsx(tabla_manual, ruta_archivo_m2)


archivos_analizados_str <-  unique(tabla_manual$origen)

# #============================================================================
#
# message("\n--- Generando análisis de resultados ---")
 manual_analizado <- analizando_resultados_manual(tabla_manual)
#
 message("\n--- Generando reporte Excel ---")
exportar_reporte_final(
  tabla = tabla_manual,
  analisis = manual_analizado,
  ruta_archivo = file.path(path_proyecto, "Reporte_del_manual_v2.xlsx"),
  archivos_analizados = archivos_analizados_str
)
message(paste("\n✅ Reporte generado con éxito en:", path_proyecto))

rm(manual_analizado)



# =============================================================================
# puedo seguir con el analisis aca con tabla_manual


# traigo tabla final de v2
tabla_final_v2 <- read_xlsx("C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/Reporte_v2.xlsx",
                            sheet="Tabla_Pacientes" )%>%
  select(origen, cama, nombre, edad, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, hb_inicial, decision) %>%
  mutate(origen = str_remove(origen, "\\.txt$"))

tabla_final_manual <- read_xlsx("C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/Reporte_del_manual_v2.xlsx",
                            sheet="Tabla_Pacientes" )%>%
# selecciono cols de interes
#tabla_final_manual <- tabla_manual %>% 
  select(origen = archivo, cama, nombre, edad, dni, nro_hc, f_internacion, fi_clinica_medica, f_hb_inicial, hb_inicial, decision)

#==========================================================================


# ── Funcion para normalizar tipos (por si hay fechas o números leídos distinto) ──────────
fx_normalizar <- function(df) {
  df  %>% 
    mutate(across(everything(), as.character),
           edad = as.numeric(edad),
           across(everything(), str_squish))
}

# Uso — ya no necesitás pasar cols_comparar
manual_norm <- fx_normalizar(tabla_final_manual)
final_v2_norm <- fx_normalizar(tabla_final_v2)

rm(tabla_final_manual,tabla_final_v2)

#str(final_v2_norm) 
#str(manual_norm)

nrow(manual_norm)-nrow(final_v2_norm) # 446-446 = 0

# drop duplicates de cada una 
manual_norm <- distinct(manual_norm) # 435

final_v2_norm <-distinct(final_v2_norm) # 435

nrow(manual_norm)-nrow(final_v2_norm) # 435-435 = 0

# ==============================================================================
# analisis a filas copmletas


# filas en manual que no estan en v1
no_en_v2 <- anti_join(manual_norm, final_v2_norm) # 154

# filas en v1 que no estan asi en manual 
no_en_manual <- anti_join(final_v2_norm, manual_norm) # 154


# 154 distintos 

# ============================
# analisis por seleccion de columnas 

cols_comparar <- c("origen","cama","nombre","dni")#,"cama" dif0,    "nombre"dif 8,   "dni" )
# , "nombre", "edad",
# , "nro_hc",
# "f_internacion", "fi_clinica_medica",
# "f_hb_inicial", "hb_inicial", "decision")

no_en_v2 <- anti_join(manual_norm, final_v2_norm, cols_comparar ) # 3

# filas en v1 que no estan asi en manual 
no_en_manual <- anti_join(final_v2_norm, manual_norm, cols_comparar)  # 3
 
