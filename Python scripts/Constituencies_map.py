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
        html.Div(
            id="constituency_map_container",
            style={'display': 'flex', 'flexDirection': 'row', 'height': '100vh'},
            children=[
                dcc.Graph(
                    id='constituency_plot',
                    figure={}
                ),
                dcc.Graph(
                    id='constituency_map',
                    figure={}
                )
            ]
        )
    ]

)

if __name__ == "__main__":
    app.run(debug=True)
