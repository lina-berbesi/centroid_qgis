**STEPS**
- Load polygon shapefile into QGIS
- Go to Vector-> Geometry tools -> Polygon centroid and create a centroid point layer
- Export the coordinates of the created centroid by going to Vector-> Geomtry tools -> Export/Add geometry columns
- Add two columns(Decimal - lenght 10) in the centroid attribute table <br>
            $$latitude=y(transform(centroid($geometry),'EPSG:2193','EPSG:4326'))$$ <br>
            $$longitude=x(transform(centroid($geometry),'EPSG:2193','EPSG:4326'))$$ <br>
- Export the attribute table to a csv using the 'MMQGIS' pluggin <br>
*For installing the pluggin go to Pluggins-> Manage and Install Pluggins -> search for 'MMQGIS'-> Install Pluggin. <br>
*To export go to 'MMQGIS'->Import/Export->Attribute Export to CSV file (Browse to respective directory) <br>

![qgis](onsumption_generation/qgis_centoids.png)
