# Anemias_R

<br>

## marzo 2026 <br>
- proyecto continuado en Subcarpeta conClaude <br>
- proximamamente readme y limpieza <br>

<br>

## 21 abril <br>
- /ConClaude fue deprecado por especificaciones de investigadora <br>

- /V3 es el actual <br>
  + Sin deduplicar (cometado el codigo no eliminado, pero se reporta igual archivos con campos Nombres + Dni duplicados) <br>
  + Se agrega columna con nombre de archivote fuente para trazabilidad <br>
  + Sin fecha internacion general, sin hemoglobina inicial, sin fecha de hemoglobina inicial, sin nro de historia clinica <br>
  + Sin hb no hay que definir sexo --> se eliminan ejecutar_2 y analisis_2, y generar_reporte_2 <br>
  + Se modifica, adapta reporte <br>
  + Se adapta el reconocimiento de encabezado de segmentos para nuevos formatos de archivos que vendran <br>


## 9 mayo <br>
Pendrive que aporta los 78 archivos a procesar. <br>
Se corre ejecutar.R. La segmentacion dio mas de 8 mil archivitos .txt. en 15 minutos <br>
Colapsó en la funcion evaluar_pacientes <br>
Se soluciona con ayuda de Claude. (ver funcion modificada) <br>
Se analizan los archivos obteniendose el reporte final a evaluar por la investigadora <br>
Todos los arhcivos quedan en el pendrive. 