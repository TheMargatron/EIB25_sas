# =============================================================================
# Title:        Surfers Against Sewage
# Description:  Creates leaflet map
# Author:       Margaret Bolton
# Created:      2025-07-24
# Last updated: 2025-07-24
# Dependencies: Python 3.10+
#               pandas, requests, geopandas, shapely
# Inputs:       wrangled data
# Outputs:      leaflet map
# =============================================================================

import os
from pathlib import Path
import re

import pandas as pd
import geopandas as gpd
import dash
# import dash_core_components as dcc
# import dash_html_components as html
import cufflinks as cf
from dash import dcc, html, Input, Output
import plotly.graph_objects as go
import dash_leaflet as dl
import plotly.express as px
# import requests
# import Data_wrangling

# app setup
app = dash.Dash(
    __name__,
    meta_tags=[
        {"name": "viewport", "content": "width=device-width, initial-scale=1.0"}
    ],
)

app.title = "Constituencies sewage"
server = app.server

APP_PATH = str(Path(__file__).parent.resolve())
project_root = Path(__file__).parent.parent.resolve()

# load data 
# (later will come from data wrangling, but currently still raw)
constituency_shape = (gpd.read_file(project_root / 'Data/Constituencies_July_2024/PCON_JULY_2024_UK_BSC.shp')
                        .to_crs(epsg=4326))
constituency_shape["id"] = constituency_shape.index.astype(str)
constituency_shape = constituency_shape.__geo_interface__

EDM_static_data = pd.read_csv(project_root / 'Data/Sewage events 2025.csv')

# map on left, plot on right 
# later will adapt to mobile with plot below
app.layout = html.Div(
    id = "maproot",
    children=[
        html.Div(
            style={'display': 'flex', 'flexDirection': 'column', 'width': '100vw'},
            id = "mapheader",
            children=[
                html.H2("Constituencies interactive sewage map"),
                html.P("Constituency boundaries define the area an MP represents.\
                        Sewage data is aggregated to the constituency level from water company APIs."),
            ]
        ),
        # Main container for basic map and plot
        html.Div(
            id="constituency_dash_container",
            style={'display': 'flex', 'flexDirection': 'row', 'height': '75vh'},
            children=[
                dl.Map(
                    id="constituency_map",
                    style={'width' : '65vw'},
                    children=[
                        dl.TileLayer(),
                        dl.GeoJSON(
                            id="constituencies_geojson",
                            data=constituency_shape,
                            options=dict(style=dict(fillColor="blue", color="black", weight=1, fillOpacity=0.3)),
                            hoverStyle=dict(weight=2, fill="red", fillOpacity=0.6)
                        ),
                        # empty layer for highlighted constituency
                        html.Div(id="highlight_container")
                    ],
                    center=[50.7, -3.5],
                    zoom=8
                ),
                dcc.Graph(
                    id='constituency_plot',
                    style={'width' : '30vw'},
                    figure={}
                )
            ]
        )
    ]

)


@app.callback(
    Output('highlight_container', 'children'),
    Input('constituencies_geojson', 'clickData'),
    prevent_initial_call=True
)
def update_map_with_highlight(clickData):
    # If something is clicked, add highlight layer
    if clickData is not None:
        clicked_id = clickData['id']
        constituency_name = clickData['properties'].get('PCON24NM', 'Unknown')

        # Find the clicked feature in your data
        clicked_feature = None
        for feature in constituency_shape['features']:
            if feature['id'] == clicked_id:
                clicked_feature = feature
                break
        
        if clicked_feature:
            # Add highlight layer with only the clicked constituency
            layers=[dl.GeoJSON(
                    data={'type': 'FeatureCollection', 'features': [clicked_feature]},
                    options=dict(style=dict(fillColor="yellow", color="red", weight=3, fillOpacity=0.7))
                )]
    
    return layers


if __name__ == "__main__":
    app.run(debug=True)
