# Create new file: validadores.R

validar_edad <- function(edad) {
  list(
    es_valido = !is.na(edad) && edad >= 18 && edad <= 120,
    mensaje = case_when(
      is.na(edad) ~ "Edad faltante",
      edad < 18 ~ "Edad menor a 18 (ya debe estar excluido)",
      edad > 120 ~ "Edad sospechosa (>120 años)",
      TRUE ~ "OK"
    )
  )
}

validar_hb <- function(hb) {
  list(
    es_valido = !is.na(hb) && hb >= 3.0 && hb <= 20.0,
    mensaje = case_when(
      is.na(hb) ~ "HB faltante",
      hb < 3.0 ~ "HB demasiaaado baja (<3)",
      hb > 30.0 ~ "HB demasiado alta sospechosa (>20)",
      TRUE ~ "OK"
    )
  )
}

validar_dni <- function(dni) {
  # DNI en Argentina: 7-8 dígitos
  es_numerico <- !is.na(dni) && str_detect(dni, "^\\d{7,8}$")
  list(
    es_valido = es_numerico,
    mensaje = if_else(es_numerico, "OK", "DNI formato inválido")
  )
}

validar_paciente <- function(paciente_row) {
  validaciones <- list(
    edad = validar_edad(paciente_row$edad),
    hb = validar_hb(paciente_row$hb_inicial),
    dni = validar_dni(paciente_row$dni)
  )
  
  # Add validation column
  paciente_row$validacion_issues <- paste(
    validaciones$edad$mensaje,
    validaciones$hb$mensaje,
    validaciones$dni$mensaje,
    sep = " | "
  )
  
  paciente_row$validacion_ok <- all(
    validaciones$edad$es_valido,
    validaciones$hb$es_valido,
    validaciones$dni$es_valido
  )
  
  return(paciente_row)
}