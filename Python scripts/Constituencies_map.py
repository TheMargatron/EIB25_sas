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

# prep map
# def generate_map_fig():
#     fig = go.Figure()

#     for _, row in constituency_shape.iterrows():
#         geo = row.geometry
#         if geo.geom_type == "Polygon":
#             polys = [geo]
#         elif geo.geom_type == "MultiPolygon":
#             polys = geo.geoms
#         else:
#             continue

#         for poly in polys:
#             lon, lat = poly.exterior.coords.xy
#             fig.add_trace(go.Scattermapbox(
#                 lon=lon, lat=lat,
#                 mode="lines",
#                 fill="toself",
#                 name=row["id"],
#                 customdata=[row["id"]] * len(lon),
#                 hoverinfo="text"
#             ))

#     fig.update_layout(
#         mapbox_style="carto-positron",
#         mapbox_zoom=10,
#         # mapbox_center={
#         #     "lat": constituency_shape.geometry.centroid.y.mean(),
#         #     "lon": constituency_shape.geometry.centroid.x.mean()
#         #     },
#         # clickmode="event+select",
#         margin=dict(l=0, r=0, t=0, b=0),
#         showlegend=False
#     )
#     return fig

# temporarily using scattermapbox before setting up access token
# def generate_map_fig():
#     fig = go.Figure(go.Scattermapbox(
#         lat=[], lon=[], mode='markers'  # No markers yet
#     ))

#     fig.update_layout(
#         mapbox_style="carto-positron",  # Free basemap
#         mapbox_zoom=8,                  # Zoom level
#         mapbox_center={"lat": 50.7, "lon": -3.5},  # Exeter center
#         margin={"r":0, "t":0, "l":0, "b":0}      # No whitespace
#     )

#     return fig

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
                            data=constituency_shape,
                            options=dict(style=dict(fillColor="blue", color="black", weight=1, fillOpacity=0.3)),
                            hoverStyle=dict(weight=2, fill="red", fillOpacity=0.6)
                        )
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
        # html.Div(
        #     id="constituency_map_container",
        #     style={'display': 'flex', 'flexDirection': 'row', 'height': '90vh'},
        #     children=[
        #         dcc.Graph(
        #             id='constituency_plot',
        #             style={'width' : '30vw'},
        #             figure={}
        #         ),
        #         dcc.Graph(
        #             id='constituency_map',
        #             style={'width' : '70vw'},
        #             figure=generate_map_fig()
        #         )
        #     ]
        # )
    ]

)

# Callback to highlight constituency on hover
# @app.callback(
#     Output('constituency_map', 'style'),
#     Input('constituency_map', 'hoverData')
# )
# def highlight_constituency(hoverData):
#     if hoverData is None:
#         return {'width': '65vw'}
#     else:
#         # Get the ID of the hovered constituency
#         constituency_id = hoverData['points'][0]['customdata'][0]
#         return {
#             'width': '65vw',
#             'highlighted': constituency_id
#         }

# @app.callback(
#     Output('constituency_map', 'figure'),
#     Input('constituency_map', 'id')
# )
# def create_map(_):
#     fig = go.Figure()

#     # Add constituency boundaries
#     for idx, row in constituency_shape.iterrows():
#         fig.add_trace(go.Scattermap(
#             mode="lines",
#             lon=[], lat=[],  # You'd need to extract coordinates from geometry
#             name=row['PCON24NM'],
#             customdata=[row['PCON24NM']],
#             hovertemplate="<b>%{customdata}</b><extra></extra>"
#         ))
    
#     fig.update_layout(
#         mapbox=dict(
#             style="open-street-map",
#             center=dict(lat=50.7, lon=-3.5),
#             zoom=8
#         ),
#         showlegend=False
#     )
#     return fig

if __name__ == "__main__":
    app.run(debug=True)
