
exportar_reporte_final_2 <- function(tabla, analisis_final, ruta_archivo, archivos_analizados_str) {
  
  # Construir hoja de Definiciones
  definiciones <- tribble(
    ~Concepto , ~Definicion,
    "","",
    "Archivos analizados:", archivos_analizados_str,
    "Nro de sub-archivos:", as.character(analisis_final$counts$n_archivos),
    "Pacientes únicos (Nombre+DNI):", as.character(analisis_final$counts$n_dni),
    "---", "---",
    "","",
    "Hoja:","Tabla Pacientes",
    "archivo", "Nombre de archivo: compuesto por el número de cama del paciente, guión bajo, y un código alfanumérico único y rándom de 5 caracteres, que es generado cada vez que se corre el código",
    "nombre", "Nombre y apellido del paciente (o apellido y nombre) como esté escrito en el archivo",
    "edad","Edad en años, registrada en el archivo (número entre paréntesis al lado del nombre)",
    "dni","Número de DNI registrado en el archivo",
    "nro_hc","Nro de historia clínica: 'HC, registrada en el archivo",
    "f_internacion","Fecha de internación: 'FI', registrada en el archivo",
    "fi_clinica_medica","Fecha de internación en clínica mpedica: 'FICM', registrada en el archivo",
    "f_hb_inicial","Fecha del primer laboratorio, del cual se extre el valor de hb_inicial",
    "hb_inicial","Valor de hb obtenido del primer laboratorio: encontrado justo después del titulo 'LABORATORIO:', tomando el segundo valor de la cadena de valores separados por barras",
    "sexo","Luego de la evaluacion aparecerá un M (de masculino) o F (de femenino), en aquellos pacientes que requirieron validacion para confirmar si es anemia basal (M<13) o no (F<12). En el caso masculino, el comentario se actualiza a ánemia basal. El femenino quedará sin comentario.",
    "comentario_2","Tras la revision manual de la tabla se combina el comentario previo con lo nuevo segun sexo",
    "decision","Si comentario_2 es apto, podria CONTINUAR la evaluacion, si tiene algun motivo de exclusión, la decision será EXLUIR",
    "","",
    "---", "---",
    "RESULTADOS GENERALES", "",
    "Registros EXCLUIDOS por motivos clínicos:", as.character(analisis_final$counts$excluidos),
    "Registros CONTINUAR:", as.character(analisis_final$counts$continuan)
  )
  # Calidad de datos
  quality_report_2 <- generar_reporte_calidad(tabla_ok, extraction_log)
  
  # Crear lista de hojas
  hojas <- list(
    "Definiciones" = definiciones,
    "Tabla_Pacientes" = tabla_ok,
    "Vacios" = analisis_final$vacios,
    "Biostats_Gral" = analisis_final$bio1,
    "Biostats_Decision" = analisis_final$bio2,
    "Resumen_Exclusion" = analisis_final$excluidos,
    "HB_Totales" = analisis_final$hb_tot,
    "HB_Anemias" = analisis_final$hb_anemia,
    "HB_Sin_Anemia" = analisis_final$hb_sin_anemia,
    "Calidad_Datos" = quality_report_2$extraccion,
    "Duplicados" = quality_report_2$duplicados
  )
  message("--- Generando reporte final ---")
  write_xlsx(hojas, ruta_archivo)
  message(paste("Reporte generado exitosamente en:", ruta_archivo))
}

