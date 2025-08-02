# SAS Environmental Impact Dashboard

**EI Grand Challenge 2025 - Surfers Against Sewage Project**

A comprehensive data visualization and monitoring platform for tracking Combined Sewer Overflow (CSO) events and sewage spills across UK water company regions. This project provides interactive maps and dashboards to visualize sewage discharge events, helping communities understand and respond to water pollution in their areas.

## 🌊 Project Overview

This project develops interactive Shiny dashboards and data processing pipelines to:
- **Monitor sewage spill events** from water company APIs
- **Track spill duration and frequency** by constituency and site
- **Provide constituency-level summaries** for local advocacy
- **Enable data-driven environmental stewardship**

Please note, it is still under active development so some features are still incomplete. If you have any suggestions, disagreements, or problems, please raise an issue.

## 🗂️ Project Structure

```
📦 EIB25_sas/
├── 📁 Data/                          # Raw data and shapefiles
│   ├── Constituencies_July_2024/     # UK constituency boundaries (shapefiles)
│   ├── CSO Database.xlsx             # Combined Sewer Overflow database
│   ├── Sewage events 2025.csv        # Current sewage event data
│   └── Fake_Sewage Events.csv        # Test/demo data
├── 📁 R scripts/                     # R analysis and Shiny apps
│   ├── SAS shiny dashboard.R         # Main constituency-level dashboard
│   ├── Sites app.R                   # Site-specific visualization app
│   ├── Constituencies_map.py         # Constituency mapping functions
│   ├── Data_wrangling.py             # Data processing functions
│   └── requirements.txt              # R package dependencies
├── 📁 Python scripts/                # Python data processing
│   ├── Data_wrangling.py             # Main data processing pipeline
│   ├── Constituencies_map.py         # Map generation scripts
│   └── requirements.txt              # Python dependencies
└── 📁 Outputs/                       # Generated visualizations and data
    ├── constituency_map.html          # Interactive constituency map
    ├── CSO_points.geojson            # CSO location data
    ├── EDM_points.geojson            # Event Duration Monitoring data
    └── MP_data.csv                   # MP contact information
```

## 📊 Features

### 🗺️ Interactive Maps
- **Clickable constituency polygons** showing sewage event summaries
- **CSO site markers** with spill history visualization
- **River network highlighting** when sites are selected
- **Custom styling** with SAS brand colors

### 📈 Data Visualizations
- **Timeline plots** showing spill events, maintenance, and normal operation
- **Summary statistics** by constituency (events, duration, affected sites)
- **Interactive filtering** by water company and time period

### 🏛️ Constituency Integration
- **MP contact information** integration
- **Local advocacy support** with pre-formatted data summaries
- **Geospatial analysis** linking spills to political boundaries

## 📄 License

This project is developed for environmental advocacy and research purposes. Please ensure appropriate attribution when using or adapting this code.

---
