# Energy Efficiency & Deprivation in Haringey — En10ergy Retrofit Explorer

## Background & Purpose
This app was created for **En10ergy**, a zero-carbon community energy social enterprise serving Muswell Hill, Hornsey, and Wood Green. Its purpose is to help highlight where **low energy efficiency homes (EPC A–G)** overlap with **high deprivation levels (IMD decile 1–2)**—to better inform where retrofit support and community outreach are most needed.

While En10ergy offers support around **solar PV and LED lighting** in their local projects, this tool focuses on visualising **LED recommendations** in the data (flagged separately) versus all other improvements grouped together, to help prioritise where simple, cost-effective actions might make the most difference.

For outreach planning, the app overlays **community centre locations**, allowing teams to visually see which hubs are near clusters of less efficient homes—so onsite advice or promotion can be more effectively targeted.

The app also includes **tenure type** (social rental vs otherwise), which helps En10ergy better understand which households might be more accessible or appropriate for retrofit interventions.

---

## Live Demo
🔗 [View the interactive app](https://pingyuchen.shinyapps.io/epc_interactive_map/)

---

## How to Use
1. **Select EPC bands** (A = most efficient → G = least) via checkboxes.
2. **Adjust IMD decile filter** (slider from 1 = most deprived up to 10 = least).
3. The **map updates** to show LSOA areas shaded by the percentage of EPC-inspected households matching the filters.
4. Optional overlays:
   - **Community centres**
   - **LED recommendation markers**
   - **Tenure type (social rental vs others)**
5. Explore the **charts**:
   - EPC efficiency trends over time (current vs potential)
   - Common retrofit measures
   - Breakdown of recommendations and ratings by tenure

---

## Data Sources
- **EPC Open Data (England & Wales)**
  - Latest dataset released July 31, 2025 (covers EPCs up to June 2025).
  - [Overview](https://epc.opendatacommunities.org/) | [API documentation](https://epc.opendatacommunities.org/docs/api/domestic)

- **Index of Multiple Deprivation (IMD) 2019**
  - LSOA-level deprivation metric.
  - [CDRC summary](https://data.cdrc.ac.uk/dataset/index-multiple-deprivation-imd)
  - [DLUHC FAQ PDF](https://assets.publishing.service.gov.uk/media/5dfb3d7ce5274a3432700cf3/IoD2019_FAQ_v4.pdf)

- **Community centre locations** were compiled from En10ergy’s internal sources for outreach planning.

---

## How It Was Built
### 1. Data Cleaning Script (`/scripts/data_cleaning.R`)
- Loads raw EPC certificate and recommendation data, postcode lookups, IMD, and community centre data.
- Recode recommendations: “LED lighting” flagged separately; all others grouped.
- Spatially join EPC postcodes to LSOA polygons, attach IMD, tenure, and retrofit flags.
- Outputs:
  - `pt.csv` — EPC points with ratings, LEDe flag, tenure, IMD, lon/lat.
  - `epc_stack.csv` — Time-series of current EPC band proportions.
  - `epc_stack_potential.csv` — Potential EPC band proportions over time.
  - `recommendations_ratio.csv` — Top retrofit measures by households.

### 2. Shiny App Script (`app.R`)
- Built with **R Shiny**, using:
  - `leaflet` (and `leaflet.extras`) for mapping.
  - `ggplot2` for charts.
- Reads the processed data files and renders the interactive map and plots.
- Deploys to shinyapps.io for live access.

---

## Running Locally
```r
install.packages(c(
  "shiny", "leaflet", "leaflet.extras", "sf", "dplyr",
  "tidyr", "lubridate", "ggplot2", "readr"
))
shiny::runApp("app.R")
```

## Limitations & Notes
- **EPC coverage**: Certificates are only issued when properties are built, sold, or rented. The dataset is not a complete record of all homes.  
- **IMD relativity**: IMD deciles measure relative deprivation; they do not give an absolute poverty score.  
- **Privacy considerations**: Point-level EPC data was aggregated to LSOA where possible, and map markers are jittered to avoid precise household identification.  
- **Simplified retrofit categories**: Only LED lighting was singled out (because it is explicitly flagged in EPC data). All other recommendations are grouped under “general improvements.”  
- **Tenure classification**: Tenure type is derived from EPC records and may not always be up-to-date or complete.  
- **Open Data Terms**: Data usage is subject to EPC Open Data licensing and attribution requirements.  

