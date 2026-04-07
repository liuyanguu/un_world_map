# UN World Map - IGME Cartography Framework

R scripts for creating publication-ready child mortality maps using UN cartography standards and Robinson projection, for UNICEF IGME reports.

Shapefiles are from 2012 UN cartography and have been used consistently for IGME reporting.

## Directory Structure

```
un_world_map/
├── shp/        # Raw UN cartography shapefiles (2012)
├── rds/        # Processed spatial objects for faster loading
├── input/      # IGME mortality estimates and country metadata
├── fig/        # Generated map outputs (PNG/PDF)
├── R/          # Core mapping function (make.world.map.R)
└── code/       # Example scripts
    ├── _Maps_of_mortality_rates_region_or_world.R        # World/regional mortality maps
    ├── _Maps_of_mortality_rates_region_or_world_bubble.R # Bubble variant
    ├── _Maps_plot_categorical_country_groups.R           # Categorical grouping maps
    ├── _Maps_plot_one_country_group.R                    # Single group highlight
    ├── script_process_sp_to_rds.R                        # Shapefile preprocessing
    ├── script_plot world sp files.R                      # Shapefile testing
    └── IGME_standardmaps_old original script.R           # Legacy (deprecated)
```

## Quick Start

Source one of the top-level scripts to generate standard IGME mortality maps:

```r
source("_world map of mortality rates.R")        # Standard mortality rate maps
source("_world map of any continuous variable.R") # Any continuous variable
```

Both scripts default to generating U5MR and MR1t59 maps; edit indicator codes at the bottom to generate others. Output PNGs are saved to `fig/`.

## Key Function: `make.world.map()`

Defined in `R/make.world.map.R`. Requires spatial objects and data to be loaded in the calling environment (handled by the example scripts).

| Parameter | Description |
|---|---|
| `ind0` | Indicator code (e.g. `"U5MR"`, `"NMR"`) |
| `iso.subset.c` | ISO3 codes for regional maps; `NULL` = world map |
| `region.name` | Label for regional maps |
| `my_color_palette` | Override default color palette |
| `my_category_break` | Override default legend breaks |
| `plot.coastlines` | Include coastlines (default: `FALSE`) |

## Supported Indicators

| Code | Description |
|---|---|
| U5MR | Under-five mortality rate |
| NMR | Neonatal mortality rate |
| CMR | Child mortality rate (1–4 years) |
| MR1t11 | 1–11 months mortality rate |
| MR1t59 | 1–59 months mortality rate |
| 5q5 / 5q10 | Mortality rate (5–9 / 10–14 years) |
| 10q10 | Adolescent mortality rate (10–19 years) |
| 5q15 | Mortality rate (15–19 years) |
| SBR | Stillbirth rate |

## Dependencies

```r
library("sf")           # Spatial data (replaces legacy sp)
library("dplyr")
library("RColorBrewer")
library("data.table")
```

## Input Data

- `input/IGME_2024_rate_estimates.csv` — IGME mortality estimates
- `input/new_cnames.rds` — Country metadata (ISO/UN codes)
- `rds/*.rds` — Preprocessed UN cartography shapefiles

If `rds/` objects are missing, regenerate them with `code/script_process_sp_to_rds.R`.

## UN Cartography Standards

- **Projection**: Robinson
- **Disputed territories**: Aksai Chin rendered with striped pattern
- **Dependencies**: Greenland → Denmark; Hong Kong/Macao → China
- **Kosovo**: Dashed boundary treatment
