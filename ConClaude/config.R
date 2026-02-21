
CONFIG <- list(
  # Paths
  paths = list(
    proyecto = "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado",
    word = "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/",
    txt = "C:/Users/luisa/OneDrive/Desktop/sup_PEDRIVE/test_modularizado/"
  ),
  
  # Extraction parameters
  extraction = list(
    hb_range = c(3.0, 45.0),
    edad_minima = 18,
    edad_maxima = 120,
    lab_search_lines = 10  # How many lines to search after "LABORATORIOS:"
  ),
  
  # Regex patterns (from patterns.R)
  patterns = list(
    exclusion = "((?<!tuvo |niega |sin |de )embaraz(?!os|.*(negativo|-))|gestac(?!.*(negativo|-))|hemorrag|politrauma|trauma(?!to|log|tolo))"
  ),
  
  # Report settings
  report = list(
    include_nombre = TRUE,  # Set FALSE for publication
    excel_sheets = c("Definiciones", "Tabla_Pacientes", "Vacios", 
                     "Biostats_Gral", "Biostats_Decision", 
                     "Resumen_Exclusion", "HB_Totales", 
                     "HB_Anemias", "HB_Sin_Anemia")
  )
)

# Usage in other files
source("config.R")

path_proyecto <- CONFIG$paths$proyecto
hb_min <- CONFIG$extraction$hb_range[1]
hb_max <- CONFIG$extraction$hb_range[2]