### Please explain me step by step and file by file how does the ConClaude product work. If you can, please make me a diagram with the files whith their dependencies, inputs and outputs

## Files by category

### Infrastructure (no dependencies on other project files)
| File | Role | Defines |
|---|---|---|
| `config.R` | Central configuration | `CONFIG` list: all paths, HB range, age limits, regex |
| `librerias.R` | Package loader | All `library()` calls in one place |
| `patterns.R` | Regex constants | `PATTERN_EDAD_COMPLETO`, `PATTERN_NOMBRE_STOP_COMPLETO`, `PATTERN_EXCLUSION` |
| `logger.R` | Extraction issue tracking | `setup_logging()`, `log_extraction_issue()`, `export_extraction_log()` |
| `checkpoints.R` | Save & resume state | `save_checkpoint()`, `load_checkpoint()`, `ask_resume()` |
| `validadores.R` | Row-level data validation | `validar_edad()`, `validar_hb()`, `validar_dni()`, `validar_paciente()` |

### Logic (depend on infrastructure; define the actual data pipeline)

| File | Role | Key functions |
|---|---|---|
| `funciones.R` | All transformation logic | `docx_a_txt()`, `segmentar_pacientes()`, `evaluar_pacientes()`, `solicitar_sexo()`, `organizar_archivos()`, `revision_tabla()`, `preview_extraction()` |
| `analisis_resultados.R` | Statistical summaries | `analizando_resultados()`, `generar_reporte_calidad()` |
| `generar_reporte.R` | Excel output (Workflow 1) | `exportar_reporte_final()` → writes `Reporte.xlsx` |
| `generar_reporte_2.R` | Excel output (Workflow 2) | `exportar_reporte_final_2()` → writes `Reporte_Final.xlsx` |

### Orchestrators (source everything and run the pipeline)

| File | Role |
|---|---|
| `ejecutar.R` | **Workflow 1** — runs everything from raw `.docx` to `Reporte.xlsx` |
| `ejecutar_2.R` | **Workflow 2** — picks up from manually edited `Tabla_Pacientes.csv` |

---

## Workflow 1 — `ejecutar.R`, step by step

**Step A** `docx_a_txt()` — reads all `.docx` from the source folder via `readtext`, writes each one as a `.txt` to the same folder.

**Step B** `segmentar_pacientes()` — reads those `.txt` files, splits each one into individual patient records (detecting the `NNNN - NOMBRE` header pattern), and writes one `.txt` per patient into `Pacientes/`. Also creates the `Pacientes/Excluidos/` folder here.

**Step C** `evaluar_pacientes()` ← checkpoint `tabla_inicial`
- Loops over every file in `Pacientes/`
- Extracts: cama, nombre, edad, DNI, HC, FI, FICM, Hb inicial
- Classifies each patient into one of: `< 18 años`, `embarazo`, `hemorragia`, `traumatismo`, `anemia basal`, `falta hb inicial`, `evaluar sexo`, `sigue`
- Runs `validar_paciente()` on every row (age/Hb/DNI format checks → adds `validacion_ok` + `validacion_issues` columns)
- No files are moved at this point
- Returns `tabla_inicial`; saves checkpoint
- Shows `preview_extraction()` and asks you to confirm before continuing

**Step D** `solicitar_sexo()` ← checkpoint `tabla_final`
- Finds rows where `comentario == "evaluar sexo"` (Hb 12.0–12.9)
- Interactively asks: *"¿Es NOMBRE de sexo MASCULINO? (Y/N)"*
- Reclassifies: M + Hb < 13 → `anemia basal`; F → `sigue`
- Adds `sexo` and `decision` (EXCLUIR / CONTINUAR) columns to all rows
- Returns `tabla_final`; saves checkpoint

**Step E** `analizando_resultados()` — computes counts, missing-value table, exclusion groups, Hb descriptive stats (total / anemia / non-anemia), and biostats tables stratified by `decision`.

**Step F** `exportar_reporte_final()` — assembles 11 sheets into `Reporte.xlsx`: Definiciones, Tabla_Pacientes, Vacios, Biostats_Gral, Biostats_Decision, Resumen_Exclusion, HB_Totales, HB_Anemias, HB_Sin_Anemia, Calidad_Datos, Duplicados.

**Step G** — asks you:
- **Y → manual edit path**: program stops. You open `Reporte.xlsx`, fill in missing sexes in `Tabla_Pacientes`, export as `Tabla_Pacientes.csv`. Then run `ejecutar_2.R`.
- **N → auto-finalize**: calls `organizar_archivos()` immediately, which moves all files with `decision == "EXCLUIR"` to `Pacientes/Excluidos/`.

**Step H** — if any extraction issues were logged, exports `extraction_issues.csv`. Pipeline complete.

---

## Workflow 2 — `ejecutar_2.R`

Picks up where Workflow 1 left off (Y path):
1. Reads `Tabla_Pacientes.csv` (manually edited)
2. `revision_tabla()` — re-applies sex-based anemia logic on the completed sexes; merges with any existing `comentario` to produce `comentario_2` and a final `decision`
3. `analizando_resultados()` — recomputes all stats on updated table
4. `exportar_reporte_final()` → writes `Reporte_Final.xlsx`
5. `organizar_archivos()` — moves EXCLUIR files to `Pacientes/Excluidos/`

---

## Dependency & data-flow diagram

```
┌─────────────────────── INFRASTRUCTURE ────────────────────────┐
│  config.R ──► CONFIG (paths, params)                          │
│  librerias.R ──► all packages                                 │
│  patterns.R ──► PATTERN_EDAD, PATTERN_NOMBRE, PATTERN_EXCL   │
│  logger.R ──► extraction_log, setup_logging()                 │
│  checkpoints.R ──► save/load/ask_resume()                     │
│  validadores.R ──► validar_paciente()                         │
└───────────────────────────────────────────────────────────────┘
         ▲ sourced by
┌─────────────────────── LOGIC LAYER ───────────────────────────┐
│  funciones.R (uses patterns, validadores, logger)             │
│  analisis_resultados.R (consumes tabla_final)                 │
│  generar_reporte.R (consumes analisis object)                 │
│  generar_reporte_2.R (consumes analisis object)               │
└───────────────────────────────────────────────────────────────┘
         ▲ sourced by
┌──────────────── ORCHESTRATORS ────────────────────────────────┐
│  ejecutar.R              │  ejecutar_2.R                      │
└──────────────────────────┴────────────────────────────────────┘


DATA FLOW — Workflow 1 (ejecutar.R)
─────────────────────────────────────────────────────────────────

  [*.docx]
      │ docx_a_txt()
      ▼
  [*.txt]  (same folder)
      │ segmentar_pacientes()
      ▼
  [Pacientes/*.txt]  +  [Pacientes/Excluidos/]  (created, empty)
      │ evaluar_pacientes()  +  validar_paciente()
      ▼
  tabla_inicial  ──► checkpoint: tabla_inicial.rds
      │ preview + user confirm
      │ solicitar_sexo()  (interactive: fills sexo column)
      ▼
  tabla_final  ──► checkpoint: tabla_final.rds
      │ analizando_resultados()
      ▼
  analisis_obj
      │ exportar_reporte_final()
      ▼
  Reporte.xlsx  (11 sheets)
      │
      ├─[N]──► organizar_archivos()
      │              └─► EXCLUIR files moved to Pacientes/Excluidos/
      │                  END
      │
      └─[Y]──► PAUSE — user edits Tabla_Pacientes sheet
                    └─► Tabla_Pacientes.csv


DATA FLOW — Workflow 2 (ejecutar_2.R)
─────────────────────────────────────────────────────────────────

  Tabla_Pacientes.csv  (manually completed sexes)
      │ revision_tabla()
      ▼
  tabla_ok  (comentario_2 + updated decision)
      │ analizando_resultados()
      ▼
  analisis_final
      │ exportar_reporte_final()
      ▼
  Reporte_Final.xlsx  (same 11 sheets, updated)
      │ organizar_archivos()
      ▼
  EXCLUIR files moved to Pacientes/Excluidos/
  END
```

---

The key design principle worth highlighting: **nothing in the logic layer (`funciones.R`, etc.) knows about file paths or orchestration order** — all of that lives in the two `ejecutar` scripts. The logic layer only receives data frames and returns data frames, which makes it easy to test or reuse individual steps.

I'll suggest some questions below.


<span class="suggestion">Add a README.md to the project root with this explanation (or a Spanish version of it).</span>


<span class="suggestion">Check that `revision_tabla()` in `funciones.R` is producing `comentario_2` and `decision` correctly for all edge cases — it's the most complex logic in Workflow 2.</span>


<span class="suggestion">Review `generar_reporte_2.R` vs `generar_reporte.R` — they look nearly identical; consider whether they can be merged into a single function with a parameter.</span>





