
exportar_reporte_final <- function(tabla, analisis, ruta_archivo, archivos_analizados_str) {
  
  # Construir hoja de Definiciones
  definiciones <- tribble(
    ~Concepto , ~Definicion,
    "","",
    "Archivos analizados:", archivos_analizados_str,
    "Nro de sub-archivos:", as.character(analisis$counts$n_archivos),
    "Pacientes únicos (Nombre+DNI):", as.character(analisis$counts$n_dni),
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
    "comentario","Comentarios que hacen a la exclusión del paciente de continuar la evaluación para el estudio. En orden de jerarquia, los valores posibles son: '< 18': si la edad del paciente no es mayor o igual a 18; 'hemorragia','traumatismo' o 'embarazo': si se detectó alguna de estos término en el archivo del paciente. 'anemia basal: valor' si se detectó una hb_inicial menor a 12, y luego de la validacion de sexo, tambien si es masculino y su hb_inicial fue menor a 13",
    "decision","Si comentario es apto, podria CONTINUAL la evaluacion, si tiene algun motivo de exclusión, la decision será EXLUIR",
    "","",
    "---", "---",
    "RESULTADOS GENERALES", "",
    "Registros EXCLUIDOS por motivos clinicos:", as.character(analisis$counts$excluidos),
    "Registros CONTINUAR:", as.character(analisis$counts$continuan)
  )
  
  # Crear lista de hojas
  hojas <- list(
    "Definiciones" = definiciones,
    "Tabla_Pacientes" = tabla,
    "Vacios" = analisis$vacios,
    "Biostats_Gral" = analisis$bio1,
    "Biostats_Decision" = analisis$bio2,
    "Resumen_Exclusion" = analisis$excluidos,
    "HB_Totales" = analisis$hb_tot,
    "HB_Anemias" = analisis$hb_anemia,
    "HB_Sin_Anemia" = analisis$hb_sin_anemia
  )
  message("--- Generando reporte final ---")
  write_xlsx(hojas, ruta_archivo)
  message(paste("Reporte generado exitosamente en:", ruta_archivo))
}
