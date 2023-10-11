**STEPS:**
- Load polygon shapefile into QGIS
- Go to Vector-> Geometry tools -> Polygon centroid and create a centroid point layer
- Export the coordinates of the created centroid by going to Vector-> Geomtry tools -> Export/Add geometry columns
- Add two columns(Decimal - lenght 10) in the centroid attribute table
  **latitude** 
            $$y(transform(centroid($geometry),'EPSG:2193','EPSG:4326'))$$
  **longitude**
            $$x(transform(centroid($geometry),'EPSG:2193','EPSG:4326'))$$
- Export the attribute table to a csv using the 'MMQGIS' pluggin
            For installing the pluggin go to Pluggins-> Manage and Install Pluggins -> search for 'MMQGIS'-> Install Pluggin
            To export go to 'MMQGIS'->Import/Export->Attribute Export to CSV file (Browse to respective directory)

![qgis](qgis_centoids.png)
