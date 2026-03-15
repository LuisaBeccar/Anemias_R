PATTERN_EDAD <- list(
  parentheses = "\\(\\s*\\d{1,3}\\s*\\)",           # (72)
  parentheses_edad = "\\(\\s*edad\\s*\\d{1,3}\\s*\\)", # (edad 72)
  keyword = "edad[:\\s]*\\d{1,3}",                  # edad: 72
  dash_comma = "[-,]\\s*\\d{1,3}(?:\\s|$)",        # - 72 or , 72
  anos = "\\d{1,3}\\s*años?"                        # 72 años
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
  end = "$"                                          # end of line
)

PATTERN_NOMBRE_STOP_COMPLETO <- paste0(
  "(?i)^.+?(?=",
  paste(unlist(PATTERN_NOMBRE_STOP), collapse = "|"),
  ")"
)

# Exclusion criteria pattern (pregnancy, hemorrhage, trauma)
# Negative lookbehinds prevent false positives on negated mentions
PATTERN_EXCLUSION <- "((?<!tuvo |niega |sin |de )embaraz(?!os|.*(negativo|-))|gestac(?!.*(negativo|-))|hemorrag|politrauma|trauma(?!to|log|tolo))"