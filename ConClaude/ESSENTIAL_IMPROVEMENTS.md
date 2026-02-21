# PaulaAnemias Project - Essential Improvements & Recommendations

## 🎯 Priority Improvements

---

### 1. **CRITICAL: Add Logging & Error Tracking** ⚠️

**Current Issue:** 
- When extraction fails (edad, nombre, HB, etc.), you have no record of which files had problems
- Difficult to debug issues after the fact
- No way to track data quality metrics

**Recommended Solution:**

```r
# Create new file: logger.R

library(logger)

# Setup logging
log_threshold(INFO)
log_appender(appender_tee(file.path(path_proyecto, "pipeline.log")))

# Initialize extraction log
extraction_log <- list()

log_extraction_issue <- function(archivo, campo, valor_extraido, linea_original) {
  issue <- list(
    timestamp = Sys.time(),
    archivo = archivo,
    campo = campo,
    valor = valor_extraido,
    linea = linea_original,
    es_NA = is.na(valor_extraido)
  )
  
  extraction_log[[length(extraction_log) + 1]] <<- issue
  
  if (is.na(valor_extraido)) {
    log_warn("Failed to extract {campo} from {archivo}")
  }
}

# Export log at end
export_extraction_log <- function() {
  df <- bind_rows(extraction_log)
  write_csv(df, file.path(path_proyecto, "extraction_issues.csv"))
  
  # Summary
  log_info("Total extraction issues: {nrow(df)}")
  log_info("Missing edad: {sum(df$campo == 'edad' & df$es_NA)}")
  log_info("Missing hb_inicial: {sum(df$campo == 'hb_inicial' & df$es_NA)}")
}
```

**Usage in evaluar_pacientes():**
```r
# After extracting edad
if (is.na(edad_v)) {
  log_extraction_issue(p, "edad", edad_v, linea_1)
}

# After extracting hb
if (is.na(res_hb$valor)) {
  log_extraction_issue(p, "hb_inicial", res_hb$valor, 
                       paste(contenido[idx_lab:(idx_lab+5)], collapse = " | "))
}
```

**Benefits:**
- ✅ Track every extraction failure
- ✅ Debug specific files easily
- ✅ Measure data quality
- ✅ Identify pattern issues in source data

---

### 2. **Add Data Validation Layer** 🔍

**Current Issue:**
- No validation that extracted data makes sense
- Edad could be 999, HB could be 100, DNI could be wrong format
- Invalid data only discovered during analysis

**Recommended Solution:**

```r
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
      hb < 3.0 ~ "HB imposible (<3)",
      hb > 20.0 ~ "HB sospechosa (>20)",
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
```

**Add to pipeline:**
```r
# In evaluar_pacientes(), before return:
df_final <- df_final %>%
  rowwise() %>%
  mutate(validacion_paciente = list(validar_paciente(cur_data()))) %>%
  unnest_wider(validacion_paciente)

# Add validation summary to report
validation_summary <- df_final %>%
  summarise(
    total_pacientes = n(),
    con_issues = sum(!validacion_ok),
    edad_invalida = sum(str_detect(validacion_issues, "Edad")),
    hb_invalida = sum(str_detect(validacion_issues, "HB")),
    dni_invalido = sum(str_detect(validacion_issues, "DNI"))
  )
```

**Benefits:**
- ✅ Catch invalid data early
- ✅ Flag suspicious values for review
- ✅ Data quality metrics in report

---

### 3. **Create Checkpoint/Resume System** 💾

**Current Issue:**
- If pipeline fails at stage 5, must restart from stage 1
- Wastes time re-processing everything
- Risky with large datasets

**Recommended Solution:**

```r
# Create new file: checkpoints.R

save_checkpoint <- function(stage_name, data, path_proyecto) {
  checkpoint_dir <- file.path(path_proyecto, "checkpoints")
  if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir)
  
  checkpoint_file <- file.path(checkpoint_dir, paste0(stage_name, ".rds"))
  saveRDS(data, checkpoint_file)
  
  # Save metadata
  metadata <- list(
    stage = stage_name,
    timestamp = Sys.time(),
    rows = nrow(data),
    cols = ncol(data)
  )
  saveRDS(metadata, file.path(checkpoint_dir, paste0(stage_name, "_meta.rds")))
  
  log_info("Checkpoint saved: {stage_name}")
}

load_checkpoint <- function(stage_name, path_proyecto) {
  checkpoint_file <- file.path(path_proyecto, "checkpoints", paste0(stage_name, ".rds"))
  
  if (file.exists(checkpoint_file)) {
    log_info("Loading checkpoint: {stage_name}")
    return(readRDS(checkpoint_file))
  } else {
    log_warn("No checkpoint found for: {stage_name}")
    return(NULL)
  }
}

ask_resume <- function(stage_name, path_proyecto) {
  checkpoint <- load_checkpoint(stage_name, path_proyecto)
  
  if (!is.null(checkpoint)) {
    meta <- readRDS(file.path(path_proyecto, "checkpoints", paste0(stage_name, "_meta.rds")))
    
    cat("\n========================================\n")
    cat("CHECKPOINT FOUND:", stage_name, "\n")
    cat("Created:", as.character(meta$timestamp), "\n")
    cat("Rows:", meta$rows, "| Cols:", meta$cols, "\n")
    cat("========================================\n")
    
    response <- readline(prompt = "Resume from this checkpoint? (Y/N): ")
    
    if (toupper(response) == "Y") {
      return(checkpoint)
    }
  }
  
  return(NULL)
}
```

**Add to ejecutar.R:**
```r
# After segmentation
save_checkpoint("segmentation_complete", 
                list(path_pacientes = path_pacientes), 
                path_proyecto)

# After evaluation
tabla_inicial_from_checkpoint <- ask_resume("tabla_inicial", path_proyecto)

if (is.null(tabla_inicial_from_checkpoint)) {
  tabla_inicial <- evaluar_pacientes(path_pacientes, path_excluidos)
  save_checkpoint("tabla_inicial", tabla_inicial, path_proyecto)
} else {
  tabla_inicial <- tabla_inicial_from_checkpoint
}

# After sex validation
save_checkpoint("tabla_final", tabla_final, path_proyecto)
```

**Benefits:**
- ✅ Resume from any stage if error occurs
- ✅ Skip long-running stages during debugging
- ✅ Save time with large datasets

---

### 4. **Add Progress Indicators** 📊

**Current Issue:**
- No idea how long processing will take
- Can't tell if pipeline is stuck or working
- Especially problematic for large datasets

**Recommended Solution:**

```r
# Install progress bar package
# install.packages("progress")

library(progress)

# In evaluar_pacientes()
pacientes_files <- list.files(path = path_pacientes, pattern = "\\.txt$", full.names = FALSE)

# Add progress bar
pb <- progress_bar$new(
  format = "  Evaluating [:bar] :current/:total (:percent) ETA: :eta",
  total = length(pacientes_files),
  clear = FALSE,
  width = 80
)

for (p in pacientes_files) {
  pb$tick()  # Update progress
  
  # ... rest of loop
}

# For segmentar_pacientes()
pb_seg <- progress_bar$new(
  format = "  Segmenting [:bar] :current/:total files",
  total = length(archivos_pendrive),
  clear = FALSE
)

for (archivo_txt in archivos_pendrive) {
  pb_seg$tick()
  # ... rest of loop
}
```

**Benefits:**
- ✅ Know how much time remaining
- ✅ Detect if pipeline is stuck
- ✅ Better user experience

---

### 5. **Improve Regex Pattern Documentation** 📝

**Current Issue:**
- Complex regex patterns have no inline comments
- Hard to maintain/modify later
- New team members can't understand patterns

**Recommended Solution:**

```r
# Create patterns.R file with documented regex

# Pattern for edad extraction
PATTERN_EDAD <- list(
  parentheses = "\\(\\s*\\d{1,3}\\s*\\)",           # (72)
  parentheses_edad = "\\(\\s*edad\\s*\\d{1,3}\\s*\\)", # (edad 72)
  keyword = "edad[:\\s]*\\d{1,3}",                  # edad: 72
  dash_comma = "[-,]\\s*\\d{1,3}(?:\\s|$)",        # - 72 or , 72
  anos = "\\d{1,3}\\s*años?"                        # 72 años
)

# Combine into single pattern
PATTERN_EDAD_COMPLETO <- paste(
  "(?i)",
  "(",
  paste(PATTERN_EDAD, collapse = "|"),
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

PATTERN_NOMBRE_STOP_COMPLETO <- paste(
  "(?i)^.+?(?=",
  paste(PATTERN_NOMBRE_STOP, collapse = "|"),
  ")"
)

# Usage
edad_raw <- linea_sin_cama %>% str_extract(PATTERN_EDAD_COMPLETO)
nombre_v <- linea_sin_cama %>% str_extract(PATTERN_NOMBRE_STOP_COMPLETO)
```

**Benefits:**
- ✅ Easy to understand patterns
- ✅ Easy to modify/extend
- ✅ Self-documenting code
- ✅ Can test components individually

---

### 6. **Add Unit Tests for Extraction Functions** 🧪

**Current Issue:**
- No automated testing
- Changes might break extraction
- Manual testing is time-consuming

**Recommended Solution:**

```r
# Create tests/test_extraction.R

library(testthat)

test_that("extraer_edad works with various formats", {
  # Test parentheses format
  expect_equal(
    extraer_edad_from_line("GARCIA MARIA (72) DNI: 12345"),
    72
  )
  
  # Test dash format
  expect_equal(
    extraer_edad_from_line("LOPEZ JUAN - 65 HC: 123"),
    65
  )
  
  # Test "años" format
  expect_equal(
    extraer_edad_from_line("PEREZ CARLOS 80 años DNI: 456"),
    80
  )
  
  # Test missing
  expect_true(
    is.na(extraer_edad_from_line("RODRIGUEZ ANA DNI: 789"))
  )
})

test_that("extraer_hb_inicial validates range", {
  # Valid HB
  contenido <- c("LABORATORIOS:", "12/05: 11.5/150/...")
  expect_equal(
    extraer_hb_inicial(contenido, 1)$valor,
    11.5
  )
  
  # Invalid HB (too high)
  contenido_invalid <- c("LABORATORIOS:", "12/05: 99.9/150/...")
  expect_true(
    is.na(extraer_hb_inicial(contenido_invalid, 1)$valor)
  )
})

test_that("extraer_nombre removes file extensions", {
  expect_equal(
    extraer_nombre_from_line("GARCIA MARIA EPICRISIS.DOCX (72)"),
    "GARCIA MARIA"
  )
  
  expect_equal(
    extraer_nombre_from_line("LOPEZ JUAN.DOCX - 65"),
    "LOPEZ JUAN"
  )
})

# Run tests
test_dir("tests")
```

**Benefits:**
- ✅ Catch regression bugs
- ✅ Safe to refactor code
- ✅ Document expected behavior
- ✅ Fast automated testing

---

### 7. **Separate Configuration from Code** ⚙️

**Current Issue:**
- File paths hardcoded in ejecutar.R
- Must edit code to change paths
- Hard to run on different machines/environments

**Recommended Solution:**

```r
# Create config.R

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
```

**Benefits:**
- ✅ Easy to change settings
- ✅ Portable across machines
- ✅ Can have dev/prod configs
- ✅ No code editing needed

---

### 8. **Add Data Quality Report** 📈

**Current Issue:**
- No overview of extraction success rate
- Don't know % of missing data
- Can't track quality over time

**Recommended Solution:**

```r
# Add to analisis_resultados.R

generar_reporte_calidad <- function(tabla, extraction_log) {
  
  # Extraction success rates
  extraccion_stats <- tibble(
    campo = c("cama", "nombre", "edad", "dni", "hb_inicial", "nro_hc", 
              "f_internacion", "fi_clinica_medica"),
    total = nrow(tabla),
    extraidos = c(
      sum(!is.na(tabla$cama)),
      sum(!is.na(tabla$nombre)),
      sum(!is.na(tabla$edad)),
      sum(!is.na(tabla$dni)),
      sum(!is.na(tabla$hb_inicial)),
      sum(!is.na(tabla$nro_hc)),
      sum(!is.na(tabla$f_internacion)),
      sum(!is.na(tabla$fi_clinica_medica))
    )
  ) %>%
    mutate(
      tasa_exito = round(extraidos / total * 100, 1),
      faltantes = total - extraidos
    )
  
  # Validation stats
  validation_stats <- tabla %>%
    summarise(
      total_pacientes = n(),
      edad_valida = sum(edad >= 18 & edad <= 120, na.rm = TRUE),
      hb_valida = sum(hb_inicial >= 3 & hb_inicial <= 20, na.rm = TRUE),
      dni_valido = sum(str_detect(dni, "^\\d{7,8}$"), na.rm = TRUE)
    )
  
  # Duplicates analysis
  duplicates_stats <- tabla %>%
    group_by(nombre, dni) %>%
    filter(n() > 1) %>%
    summarise(
      n_duplicados = n(),
      archivos = paste(archivo, collapse = ", ")
    )
  
  return(list(
    extraccion = extraccion_stats,
    validacion = validation_stats,
    duplicados = duplicates_stats
  ))
}

# Add to report
quality_report <- generar_reporte_calidad(tabla_final, extraction_log)

# Add sheet to Excel
hojas$Calidad_Datos <- quality_report$extraccion
hojas$Duplicados <- quality_report$duplicados
```

**Benefits:**
- ✅ Monitor data quality
- ✅ Identify problematic fields
- ✅ Track improvements over time
- ✅ Quality assurance for publication

---

### 9. **Add Interactive Data Preview** 👁️

**Current Issue:**
- Must open Excel to see results
- Hard to quickly check if extraction worked
- No preview before generating full report

**Recommended Solution:**

```r
# Install DT for interactive tables
# install.packages("DT")

library(DT)

# Add preview function
preview_extraction <- function(tabla, n_rows = 20) {
  
  cat("\n========================================\n")
  cat("PREVIEW: First", n_rows, "patients\n")
  cat("========================================\n\n")
  
  # Console preview
  print(head(tabla, n_rows))
  
  # HTML interactive table (opens in browser)
  tabla %>%
    head(n_rows) %>%
    datatable(
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      caption = paste("Preview of", nrow(tabla), "total patients")
    ) %>%
    # Save to temp HTML
    saveWidget(file = tempfile(fileext = ".html"), selfcontained = TRUE)
}

# Usage in ejecutar.R
# After evaluar_pacientes
preview_extraction(tabla_inicial, n_rows = 50)

response <- readline(prompt = "Continue with analysis? (Y/N): ")
if (toupper(response) != "Y") {
  stop("Pipeline stopped by user after preview")
}
```

**Benefits:**
- ✅ Quick visual check
- ✅ Catch obvious errors early
- ✅ Better user feedback

---

### 10. **Create README with Examples** 📚

**Current Issue:**
- No documentation for input file format
- New users don't know what format is expected
- Hard to prepare data correctly

**Recommended Solution:**

Create comprehensive `README.md`:

```markdown
# PaulaAnemias - Pipeline Documentation

## Input File Format

### Expected .docx/.txt Structure:

```
4254 - GARCIA MARIA (72) DNI: 12345678 HC: 987654 FI: 01/15 FICM: 01/16

LABORATORIOS:
12/05: 11.5/150/...
13/05: 10.8/145/...

[Additional patient notes...]

4255 - LOPEZ JUAN edad: 65 DNI: 87654321 HC: 123456 FI: 01/20

LABORATORIOS:
15/05: 9.5/140/...
```

### Supported Age Formats:
- `(72)` - Age in parentheses
- `edad: 72` - With keyword
- `72 años` - With "años"
- `- 72` - After dash

### Extraction Rules:
1. **Bed number (cama)**: First 4 digits
2. **Name**: Text between bed number and age
3. **Age**: See formats above
4. **Clinical fields**: DNI:, HC:, FI:, FICM:
5. **Hemoglobin**: 2nd value after "LABORATORIOS:" date

### Exclusion Criteria:
- Age < 18
- Keywords: embarazo, hemorragia, trauma
- HB < 12 (females) or < 13 (males)

## Running the Pipeline

### Workflow 1 (Initial):
```r
source("ejecutar.R")
```

### Workflow 2 (Final):
```r
# After editing Tabla_Pacientes in Google Sheets
source("ejecutar_2.R")
```

## Output Files

- `Reporte.xlsx` - Initial report
- `Reporte_Final.xlsx` - Final report (after manual review)
- `extraction_issues.csv` - Log of extraction problems
- `pipeline.log` - Complete execution log
```

**Benefits:**
- ✅ Clear documentation
- ✅ Onboarding new users
- ✅ Standardize input format
- ✅ Troubleshooting guide

---

## 🎯 Priority Matrix

| Improvement | Impact | Effort | Priority |
|------------|--------|--------|----------|
| 1. Logging & Error Tracking | HIGH | MEDIUM | **🔴 CRITICAL** |
| 2. Data Validation | HIGH | LOW | **🔴 CRITICAL** |
| 3. Checkpoints | MEDIUM | MEDIUM | **🟡 HIGH** |
| 4. Progress Indicators | LOW | LOW | **🟢 MEDIUM** |
| 5. Pattern Documentation | MEDIUM | LOW | **🟡 HIGH** |
| 6. Unit Tests | MEDIUM | HIGH | **🟡 HIGH** |
| 7. Configuration File | HIGH | LOW | **🔴 CRITICAL** |
| 8. Quality Report | HIGH | MEDIUM | **🟡 HIGH** |
| 9. Interactive Preview | LOW | LOW | **🟢 MEDIUM** |
| 10. README | MEDIUM | LOW | **🟡 HIGH** |

---

## 🚀 Recommended Implementation Order

### Phase 1 (Do First):
1. **Configuration file** (easiest, immediate benefit)
2. **Logging system** (catches issues immediately)
3. **Data validation** (prevents bad data from propagating)

### Phase 2 (Do Soon):
4. **Pattern documentation** (makes maintenance easier)
5. **Quality report** (adds value to output)
6. **README** (helps future you and collaborators)

### Phase 3 (Nice to Have):
7. **Checkpoints** (useful for large datasets)
8. **Progress indicators** (better UX)
9. **Unit tests** (when you have time)
10. **Interactive preview** (polish)

---

## 💡 Additional Considerations

### For Publication:
- Remove `nombre` and 'dni' column from final output (already noted in code)
- Add anonymization layer for other identifiable fields
- Generate summary statistics without raw data

### For Scaling:
- Consider parallel processing for large datasets
- Database instead of CSV for intermediate storage
- Automated quality checks before manual review

### For Collaboration:
- Version control (Git) for code
- Shared configuration for team
- Automated testing in CI/CD pipeline

---

## ✅ Summary

Your current pipeline is **functional and well-structured**, but these improvements would make it:
- **More robust** (logging, validation, tests)
- **Easier to maintain** (documentation, configuration)
- **More efficient** (checkpoints, progress bars)
- **Higher quality** (quality reports, validation)

**Start with Phase 1** improvements - they'll give you the biggest immediate benefit with minimal effort!