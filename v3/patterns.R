PATTERN_EDAD <- list(
  parentheses = "\\(\\s*\\d{1,3}\\s*\\)",           # (72)
  parentheses_edad = "\\(\\s*edad\\s*\\d{1,3}\\s*\\)", # (edad 72)
  keyword = "edad[:\\s]*\\d{1,3}",                  # edad: 72
  dash_comma = "[-,]\\s*\\d{1,3}(?:\\s|$)",        # - 72 or , 72
  anos = "\\d{1,3}\\s*años?",                        # 72 años
  an = "\\d{1,3}\\s*(a|añ)?",                        # 72 a or 72 añ
  antes_dni         = "\\d{1,3}(?=\\s+DNI)"   # 72 DNI  
)

# Combine into single pattern
PATTERN_EDAD_COMPLETO <- paste0(
  "(?i)(",
  paste(unlist(PATTERN_EDAD), collapse = "|"),
  ")"
)

# Pattern for nombre stop markers
PATTERN_NOMBRE_STOP <- list(
  parentheses = "\\s*\\(\\s*(?:edad\\s*)?\\d+",    # (72) or (edad 72)
  edad_keyword = "\\s+edad\\b",                     # " edad"
  anos = "\\s+\\d{2,}\\s+año",                      # "65 años"
  dash_age = "\\s*-\\s*\\d{1,3}(?:\\s|$)",        # "- 72"
  fields = "\\s+(?:DNI|HC|FI)\\b",                 # " DNI" " HC" " FI"
  end = "$",                                          # end of line
  antes_dni    = "\\s+\\d{1,3}(?=\\s+DNI)"   # " 72 DNI"
)

PATTERN_NOMBRE_STOP_COMPLETO <- paste0(
  "(?i)^.+?(?=",
  paste(unlist(PATTERN_NOMBRE_STOP), collapse = "|"),
  ")"
)

# Exclusion criteria pattern (pregnancy, hemorrhage, trauma)
# Negative lookbehinds prevent false positives on negated mentions

# --- EMBARAZO -----------------------------------------------------------------
# Detecta: "embaraz*" 
# NO detecta si está precedido por: "tuvo ", "niega ", "sin ", "de "
# NO detecta si va seguido de: "os" (ej: "embarazos previos")
#                               "negativo" o "-"
# Nota: lookbehind de ancho variable se maneja con alternativas separadas

PATTERN_EMBARAZO <- paste0(
  "(?:",
  "(?<!tuvo )(?<!niega )(?<!sin )(?<!de )(?<!no presenta )",
  "embaraz(?!os|[^\\n]{0,20}(negativo|-))",
  ")"
)

# --- GESTACIÓN ----------------------------------------------------------------
# Detecta: "gestac*"
# NO detecta si está precedido por: "niega ", "sin ", "no presenta "
# NO detecta si va seguido de: "negativo" o "-"
# NO detecta: "gestación negativa" (negación pospuesta)

PATTERN_GESTACION <- paste0(
  "(?:",
  "(?<!niega )(?<!sin )(?<!no presenta )",
  "gestac(?![^\\n]{0,20}(negativo|-))",
  ")"
)

# --- HEMORRAGIA ---------------------------------------------------------------
# Detecta: "hemorrag*"
# NO detecta si está precedido por: "niega ", "sin ", "no presenta "

PATTERN_HEMORRAGIA <- paste0(
  "(?:",
  "(?<!niega )(?<!sin )(?<!no presenta )",
  "hemorrag",
  ")"
)

# --- POLITRAUMA ---------------------------------------------------------------
# Detecta: "politrauma*"
# NO detecta si está precedido por: "niega ", "sin ", "no presenta ",
#                                    "antecedentes de ", "antecedente de "
# Se trata por separado de TRAUMA para poder distinguir motivos en el reporte

PATTERN_POLITRAUMA <- paste0(
  "(?:",
  "(?<!niega )(?<!sin )(?<!no presenta )",
  "(?<!antecedentes de )(?<!antecedente de )",
  "politrauma",
  ")"
)

# --- TRAUMA -------------------------------------------------------------------
# Detecta: "trauma*"
# NO detecta si va seguido de: "to", "log", "tolo" (traumatólogo/traumatología)
# NO detecta si está precedido por: "niega ", "sin ", "no presenta ",
#                                    "antecedentes de ", "antecedente de "
#                                    "poli" (ya cubierto por PATTERN_POLITRAUMA)

PATTERN_TRAUMA <- paste0(
  "(?:",
  "(?<!niega )(?<!sin )(?<!no presenta )",
  "(?<!antecedentes de )(?<!antecedente de )",
  "(?<!poli)",
  "trauma(?!to|log|tolo)",
  ")"
)

#==============================================================================
# PATRÓN COMBINADO
#==============================================================================

PATTERN_EXCLUSION <- paste0(
  "(",
  paste(
    PATTERN_EMBARAZO,
    PATTERN_GESTACION,
    PATTERN_HEMORRAGIA,
    PATTERN_POLITRAUMA,
    PATTERN_TRAUMA,
    sep = "|"
  ),
  ")"
)
