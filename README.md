---
editor_options: 
  markdown: 
    wrap: 72
---

# UN World Map - IGME Cartography Framework

A comprehensive R package for creating child mortality maps using UN
cartography standards. This framework enables the creation of
publication-ready world maps for UNICEF's Inter-agency Group for Child
Mortality Estimation (IGME) reports.

## Overview

This package provides tools for visualizing child mortality indicators
on world maps using official UN cartography standards and Robinson
projection. The framework supports all major IGME indicators including
under-five mortality rate (U5MR), neonatal mortality rate (NMR), and
other age-specific mortality measures.

**Key Features:** - UN cartography compliance with official boundaries
and projections - Support for all IGME mortality indicators -
Publication-ready outputs for reports and websites - Modern spatial data
handling with `sf` package - Flexible color schemes and legend
customization - Special handling for disputed territories and
dependencies

The source shape files were from 2012 UN cartography and have been used
consistently for IGME reporting.

## Directory Structure

```         
un_world_map/
├── shp/                    # Raw UN cartography shapefiles (2012)
├── rds/                    # Processed spatial objects for faster loading
├── input/                  # IGME mortality estimates and country metadata
├── fig/                    # Generated map outputs (PNG/PDF)
├── R/                      # Core mapping functions
├── code/                   # Analysis scripts and examples
│   ├── _world map of mortality rates.R           # 🌟 MAIN EXAMPLE SCRIPT
│   ├── _Maps_of_mortality_rates_by_region.R      # Regional mapping
│   ├── _Maps_plot_selected_countries.R           # Country highlighting
│   ├── _Maps_plot_selected_countries_groups.R    # Country grouping
│   ├── plot_selected_countries_groups_by_available_data_types.R  # Data availability
│   ├── script_process_sp_to_rds.R                # Shapefile preprocessing
│   ├── script_plot world sp files.R              # Shapefile testing
│   └── IGME_standardmaps_old original script.R   # Legacy (deprecated)
└── README.md
```

### Quick Start

**Main Entry Point:** Use `_world map of mortality rates.R` to generate
standard IGME mortality maps.

## Usage Examples

### Basic World Map Creation

``` r
# Load the main example script
source("_world map of mortality rates.R")

# This automatically generates maps for key indicators:
# - U5MR (Under-five mortality rate)
# - MR1t59 (1-59 months mortality rate)
```

### Custom Indicator Mapping

``` r
# Load the framework
source("_world map of mortality rates.R")

# Generate maps for specific indicators
make.world.map(ind0 = "NMR")     # Neonatal mortality rate
make.world.map(ind0 = "CMR")     # Child mortality rate (1-4 years)
make.world.map(ind0 = "SBR")     # Stillbirth rate

# Generate all available indicators
invisible(lapply(inds, make.world.map))
```

### Regional Maps

``` r
# Source the regional mapping script
source("code/_Maps_of_mortality_rates_by_region.R")

# Create regional map for specific countries
region0 <- "Sub-Saharan Africa"
isos <- dc[SDGSimpleRegion2 == region0, unique(ISO3Code)]
make.world.map(ind0 = "U5MR", iso.subset.c = isos, region.name = region0)
```

### Robinson Projection Example

``` r
library("ggplot2")
library("sf")

# Load and project shapefile
shp_file_dir <- file.path("shp/un-world-2012-no-antartica-10pct.shp")
geo <- sf::st_read(shp_file_dir)
geo <- sf::st_transform(geo, crs = "+proj=robin")  # UN standard projection

ggplot(data = geo) +
  geom_sf(fill = "grey", color = "#69b3a2") +
  theme_bw() + coord_sf()
```

![UN World Map - Robinson
Projection](fig/un-world-2012-no-antartica-10pct.png)

## Available Mortality Indicators

| Code   | Description                             | Color Palette |
|--------|-----------------------------------------|---------------|
| U5MR   | Under-five mortality rate               | Blues         |
| NMR    | Neonatal mortality rate                 | Greens        |
| CMR    | Child mortality rate (1-4 years)        | OrRd          |
| MR1t11 | 1-11 months mortality rate              | OrRd          |
| MR1t59 | 1-59 months mortality rate              | OrRd          |
| 5q5    | Mortality rate (5-9 years)              | Greens        |
| 5q10   | Mortality rate (10-14 years)            | Greens        |
| 10q10  | Adolescent mortality rate (10-19 years) | OrRd          |
| 5q15   | Mortality rate (15-19 years)            | PuBu          |
| SBR    | Stillbirth rate                         | Blues         |

## Dependencies

``` r
# Core packages
library("sf")              # Modern spatial data handling
library("ggplot2")         # Advanced visualization
library("dplyr")           # Data manipulation
library("RColorBrewer")    # Professional color palettes
library("data.table")      # Efficient data operations

# Optional packages
library("ggpattern")       # Pattern fills for disputed territories
```

## Key Functions

### `make.world.map()`

Main function for creating mortality rate maps.

**Parameters:** - `ind0`: Mortality indicator code (e.g., "U5MR",
"NMR") - `iso.subset.c`: ISO3 country codes for regional maps (NULL =
world map) - `region.name`: Descriptive name for regional maps -
`plot.coastlines`: Include detailed coastlines (default: FALSE) -
`save.pdf.also`: Save PDF in addition to PNG (default: FALSE)

## Data Requirements

### Input Files

-   `input/IGME_202x_rate_estimates.csv`: Latest IGME mortality
    estimates (only the most recent year for the map)
-   `input/new_cnames.rds`: Country metadata with ISO/UN codes
-   `rds/*.rds`: Processed UN cartography shapefiles

### Spatial Data

-   World country polygons in Robinson projection
-   UN-compliant country boundaries and disputed territory handling
-   Lakes, coastlines, and special territorial boundaries

## UN Cartography Standards

This framework follows official UN cartography guidelines:

-   **Projection**: Robinson projection for global maps
-   **Disputed Territories**: Special handling for Aksai Chin (striped
    pattern)
-   **Dependencies**: Greenland → Denmark, Hong Kong/Macao → China
-   **Kosovo**: Dashed boundary treatment
-   **Color Standards**: Accessibility-compliant ColorBrewer palettes

## Output Formats

Maps are automatically saved to the `fig/` directory: - **PNG**:
High-resolution for reports and presentations - **PDF**: Vector format
for graphic design (optional)

## Script Documentation Updates

### 2025.10 - Major Documentation Enhancement

-   ✅ Added comprehensive headers to all scripts
-   ✅ Enhanced function documentation with parameter descriptions
-   ✅ Improved inline comments throughout codebase
-   ✅ Added usage examples and best practices
-   ✅ Created migration guidance for legacy scripts
-   ✅ Standardized code organization and structure

### Recent Updates

-   **2025.03**: Updated `make.world.map` to support both categorical
    and continuous data
-   **2024.08**: Migrated from legacy `sp` to modern `sf` package
-   **2024**: Enhanced ggplot2 integration replacing base plotting

## Troubleshooting

### Common Issues

1.  **Missing spatial data**: Run `script_process_sp_to_rds.R` to
    regenerate processed shapefiles
2.  **Projection warnings**: Ensure Robinson projection is applied
    consistently
3.  **Memory issues**: Set `plot.coastlines = FALSE` for faster
    rendering
4.  **Color mapping**: Verify indicator codes match `colors_ind`
    definitions

### Legacy Migration

-   **Old script users**: Migrate from
    `IGME_standardmaps_old original script.R` to modern alternatives
-   **sp package**: Update code to use `sf` package for spatial
    operations
-   **Base plotting**: Replace with ggplot2 for better control and
    output quality
