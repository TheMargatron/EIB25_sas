# =============================================================================
# Title:        Surfers Against Sewage
# Description:  wrangles data from water company APIs for Leaflet dashboard
# Author:       Margaret Bolton
# Created:      2025-07-24
# Last updated: 2025-07-24
# Dependencies: Python 3.10+
#               pandas, requests, geopandas, shapely
# Inputs:       aggregated static data from water company APIs
# Outputs:      Prepped data for Leaflet
# =============================================================================

# Library
import pandas as pd
import geopandas as gpd
from pathlib import Path
import folium

# read fake annual sewage data
project_root = Path(__file__).parent.parent
CSO_data = pd.read_csv(project_root / 'Data/Fake_Sewage Events.csv')

# read constituency data as of July 2024
constituency_shape = (gpd.read_file(project_root / 'Data/Constituencies_July_2024/PCON_JULY_2024_UK_BSC.shp')
                        .to_crs(epsg=4326))



# sanity checks
print(constituency_shape.columns)

# Map centred on Exeter
map_center = [50.7, -3.5]  
basic_map = folium.Map(location=map_center, zoom_start=8)

# Add constituency boundaries to the map
constituency_geosjon = folium.GeoJson(
    constituency_shape,
    name='constituencies',
    style_function=lambda x: {
        'color': 'blue',
        'weight': 1,
        'fillColor': 'blue',
        'fillOpacity': 0.1
    },
    highlight_function=lambda x: {
        'color': 'red',
    },
    popup=folium.GeoJsonPopup(
        fields=['PCON24NM'], 
        aliases=['Constituency: '],
        labels=False
    )
).add_to(basic_map)

# Add a marker for Exeter
folium.Marker(
    location=[50.7, -3.5],
    popup="Exeter"
)

constituency_geosjon.add_to(basic_map)

output_path = project_root / 'outputs' / 'constituency_map.html'
output_path.parent.mkdir(exist_ok=True)  # Create outputs folder if it doesn't exist
basic_map.save(str(output_path))