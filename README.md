
# Anemias v3

Pipeline de extracción y análisis clínico de registros informales y no estructurados de pacientes a partir de archivos `.docx` de historia clínica, para un estudio clínico retrospectivo sobre anemia desarrollada en internación

---

## Estructura de archivos

```
v3/
├── ejecutar.R            # Script principal: orquesta todo el flujo
├── funciones.R           # Funciones del pipeline (segmentación, evaluación, clasificación)
├── analisis_resultados.R # Estadísticas y reporte
├── generar_reporte.R     # Exportación a Excel
├── config.R              # Rutas y configuración general
├── patterns.R            # Expresiones regulares clínicas
├── validadores.R         # Validación de edad y DNI
├── checkpoints.R         # Sistema de retomada entre sesiones
├── logger.R              # Log de errores de extracción
└── librerias.R           # Carga de paquetes
```

---

## Pipeline

El flujo se ejecuta desde `ejecutar.R` en cinco pasos secuenciales.

### Paso A — Conversión Word → TXT (`docx_a_txt`)
Lee todos los `.docx` de la carpeta de origen con `readtext` y los guarda como `.txt` planos. Es el único paso que requiere los archivos originales; el resto trabaja sobre los `.txt`.

### Paso B — Segmentación de pacientes (`segmentar_pacientes`)
Recorre cada `.txt` grande y detecta el inicio de cada registro de paciente mediante dos condiciones combinadas: la línea empieza con 4 dígitos seguidos de texto alfabético (número de cama + nombre), y dentro de las 3 líneas siguientes aparece al menos una etiqueta clínica (`DNI`, `HC`, `FICM`, etc.). Cada registro se guarda como un `.txt` individual numerado (`00001.txt`, `00002.txt`, …). El texto previo al primer registro válido se descarta automáticamente. Al finalizar genera `_mapa_origen.csv`, que vincula cada archivo individual con el `.docx` del que proviene.

### Paso C — Evaluación clínica (`evaluar_pacientes`)
Extrae de cada archivo individual: número de cama, nombre, edad, DNI y fecha de internación en clínica médica (`FICM`). Luego busca en el texto completo criterios de exclusión: embarazo, hemorragia y traumatismo. 
Para manejar volúmenes grandes (8000+ registros) sin colapsar la memoria de R, el procesamiento se divide en **batches de 1000 archivos**. Cada batch se guarda en disco como `.rds` en la subcarpeta `_batches/` al terminar, liberando la RAM antes de continuar. Si la sesión se interrumpe, los batches ya completados se saltan automáticamente en la siguiente ejecución. Al finalizar todos los batches se combinan en una única tabla.

### Paso D — Clasificación y análisis (`clasificar` + `analizando_resultados`)
Asigna a cada paciente la decisión `CONTINUAR` o `EXCLUIR` según el comentario extraído en el paso anterior. Luego calcula conteos generales, vacíos por columna y distribución de motivos de exclusión.

### Paso E — Reporte Excel (`exportar_reporte_final`)
Genera un `.xlsx` con seis hojas: definiciones del dataset, tabla completa de pacientes, vacíos, resumen de exclusiones, métricas de extracción y duplicados detectados.

---

## Sistema de checkpoints

Entre pasos, el script guarda el estado en archivos `.rds` mediante `save_checkpoint()`. Al iniciar, `ask_resume()` detecta si existe un checkpoint previo y pregunta si retomar desde ahí. Esto permite separar la ejecución en varias sesiones sin reprocesar desde cero, útil dado el volumen de archivos.

---
