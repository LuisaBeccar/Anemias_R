
exportar_reporte_final <- function(tabla, analisis, ruta_archivo, archivos_analizados_str, extraction_log = list()) {
  
  # Construir hoja de Definiciones
  definiciones <- tribble(
    ~Concepto , ~Definicion,
    "","",
    "Archivos analizados:", archivos_analizados_str,
    "Nro de sub-archivos:", as.character(analisis$counts$n_archivos),
    "---", "---",
    "","",
    "Hoja: Tabla Pacientes","",
    "archivo", "Nombre de archivo: numero de 5 digitos asignados en orden, iniciando en 00001",
    "origen","Nombre del archivo .docx del cual se segmento el registro del paciente",
    "cama", "Numero de cama del paciente",
    "nombre", "Nombre y apellido del paciente (o apellido y nombre) como esté escrito en el archivo",
    "edad","Edad en años, registrada en el archivo",
    "dni","Número de DNI o DOC registrado en el archivo",
   # "nro_hc","Nro de historia clínica: 'HC, registrada en el archivo",
    "fi_clinica_medica","Fecha de internación en clínica mpedica: 'FICM', registrada en el archivo",
   # "sexo","Luego de la evaluacion aparecerá un M (de masculino) o F (de femenino), en aquellos pacientes que requirieron validacion para confirmar si es anemia basal (M<13) o no (F<12). En el caso masculino, el comentario se actualiza a ánemia basal. El femenino quedará sin comentario.",
    "comentario","Comentarios que hacen a la exclusión del paciente de continuar la evaluación para el estudio. En orden de jerarquia, los valores posibles son: '< 18': si la edad del paciente no es mayor o igual a 18; 'hemorragia','traumatismo' o 'embarazo': si se detectó alguna de estos término en el archivo del paciente",
    "validacion_issues", "Breve descripción de los problemas de calidad detectados en edad y dni",
    "validacion_ok","Indicador booleano de integridad del registro si los dos campos de validacoin_issues son OK: TRUE, si no: FALSE",
    "decision","Si comentario es 'seguir', podria CONTINUAR la evaluacion, si tiene algun motivo de exclusión, la decision será EXLUIR",
    "","",
    "---","---",
    "","",
    "Hoja: Vacios:","Tabla con conteo de vacios por variable",
    "Hoja: Resumen_Exlcusion","Tabla con conteo de motivos de exclusion encontrados",
    "Hoja: Calidad_Datos","Tabla con las metricas de extraccion de datos existentes en cada variable",
    "Hoja: Duplicados","Reporte de registros duplicados segun campos: nombre, dni, f_internacion, fi_clinica_medica, hb_inicial",
    "","",
    "---", "---",
    "RESULTADOS GENERALES", "",
    "Registros EXCLUIDOS:", as.character(analisis$counts$excluidos),
    "Registros CONTINUAR:", as.character(analisis$counts$continuan)
  )
  
  # Calidad de datos
  quality_report <- generar_reporte_calidad(tabla, extraction_log)
  
  # Crear lista de hojas
  hojas <- list(
    "Definiciones" = definiciones,
    "Tabla_Pacientes" = tabla,
    "Vacios" = analisis$vacios,
    "Resumen_Exclusion" = analisis$excluidos,
    "Calidad_Datos" = quality_report$extraccion,
    "Duplicados" = quality_report$duplicados
  )
  message("--- Generando reporte final ---")
  write_xlsx(hojas, ruta_archivo)
  message(paste("Reporte generado exitosamente en:", ruta_archivo))
}
