
CONFIG <- list(
  # Paths
  paths = list(
    proyecto = "D:/Archivos Anemias", # donde esta todo
    word = "D:/Archivos Anemias/", # donde estan los .doc
    txt = "D:/Archivos Anemias/archivos_txt" # crear carpeta donde ponga los archivotes txt
  ),
  
  # Extraction parameters
  extraction = list(
    hb_range = c(2.0, 35.0),
    edad_minima = 18,
    edad_maxima = 120,
    lab_search_lines = 10  # How many lines to search after "LABORATORIOS:"
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
