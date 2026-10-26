# ens_semilla

Repositorio compartido del proyecto semilla CECAN. Analiza la asociación entre
tres exposiciones y la **mortalidad** e **incidencia de cáncer**, usando la
Encuesta Nacional de Salud (ENS) 2003, 2009-10 y 2016-17 enlazada con registros
de defunciones y egresos hospitalarios (bases públicas del Departamento de
Epidemiología, MINSAL).

| Exposición | Responsable | ENS analizadas |
|---|---|---|
| Antecedentes familiares de cáncer (AF) | Constanza | 2009, 2016 |
| Síntomas depresivos | Josefa | 2003, 2009 |
| ICT | Carlos | 2003, 2009, 2016 |

Para cada exposición se estiman tasas de mortalidad a 5 y 10 años, curvas de
Kaplan-Meier, modelos de Cox (crudo, ajustado por edad y sexo, y completo) y un
meta-análisis de los HR entre encuestas. El modelo conjunto con las tres
exposiciones usa solo la ENS 2009-10, la única común a las tres.

## Estructura

```
input/
  data-raw/          Bases originales (.sav) de defunciones y egresos por ENS
  data-procesada/    Bases armonizadas, unidas y finales para análisis (.rds)
procesamiento/       Scripts de R (ver orden abajo)
output/
  graphs/            Kaplan-Meier, forest plots y meta-análisis
    mortalidad/
    incidencia/
  tables/            Tabla 1, tasas y resúmenes de Cox (.md / .xlsx)
```

## Cómo usarlo

1. Hacer **pull** antes de empezar a trabajar.
2. Abrir el proyecto en la carpeta raíz del repositorio, porque todas las rutas
   son relativas a ella.
3. La primera vez, instalar los paquetes (están comentados al inicio de
   `00_setup.R`).

### Orden de los scripts

**Preparación de datos** (ya ejecutada; sus resultados están en `data-procesada/`, **no volver a correr**)

| Script | Qué hace |
|---|---|
| `00_setup.R` | Carga los paquetes. Los demás scripts lo llaman automáticamente |
| `01_armonización.R` | Armoniza las bases de defunciones de las tres ENS |
| `02_bind_egresos_defunciones.R` | Une defunciones con egresos hospitalarios |
| `03_exclusiones.R` | Aplica criterios de exclusión (p. ej., menores de 25 años) |

**Análisis**

| Script | Qué hace |
|---|---|
| `04_etiquetado_variables.R` | Carga las bases finales y etiqueta variables. Lo llaman los scripts siguientes |
| `05_analisis_descriptivo.R` | Tabla 1 por exposición y encuesta |
| `06.1` / `06.2` / `06.3` | Tasas de mortalidad general y por cáncer a 5 y 10 años (depresión / ICT / AF) |
| `07.1` / `07.2` / `07.3` | Kaplan-Meier y Cox para **mortalidad** (depresión / ICT / AF) |
| `07.4_HR_conjunto.R` | Cox conjunto con las tres exposiciones (ENS 2009-10) |
| `08.1` / `08.2` / `08.3` | Meta-análisis de mortalidad (depresión / ICT / AF) |
| `09.0_Definir incidencia.R` | Define el evento de incidencia: el primero entre egreso y defunción por cáncer |
| `10.1` / `10.2` / `10.3` | Kaplan-Meier y Cox para **incidencia** |
| `11.1` / `11.2` / `11.3` | Meta-análisis de incidencia |

Cada script de análisis llama a los que necesita con `source()`, así que se
pueden correr por separado.

## Paquetes principales

`haven`, `tidyverse`, `labelled`, `gtsummary`, `survey`/`srvyr`, `survival`,
`survminer`, `meta`, `knitr`.

## Convenciones

- Los gráficos se guardan en `output/graphs/mortalidad/` o `output/graphs/incidencia/`.
- Las tablas se guardan en `output/tables/<exposición>/`.
- Prefijos de archivos: `KM_` (Kaplan-Meier), `FP_HR_` (forest plot de HR),
  `Metaanalisis_`, `tabla1_`, `tabla_cox_resumen_`.