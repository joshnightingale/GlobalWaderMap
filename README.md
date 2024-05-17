# Hex Grid Summary

MoveApps

Github repository: [github.com/joshnightingale/GlobalWaderMap](https://www.github.com/joshnightingale/GlobalWaderMap)

## Description
Interactive map of track summary statistics presented on hexagonal grid.

## Documentation
Function to summarise large amounts of tracking data on a hexagonal grid, including layers of numbers of locations, individuals and species. Plotted on a scrollable/zoomable htmlwidget map via mapview & leaflet.

Originally intended for plotting the [Global Wader](https://www.globalwader.org) database.

### Input data
Tracking data in move2 format. Optionally, a background map area.

### Output data
move2 object. Interactive htmlwidget map as .html artefact.

### Artefacts
`map.html`: htmlwidget interactive map.

### Settings 
*Please list and define all settings/parameters that the App requires to be set by the App user, if necessary including their unit.*

`Grid size` (radius): Size of hexagonal grid cells (larger cell = slower map). Consult the table of resulutions at [github.com/r-barnes/dggridR](https://github.com/r-barnes/dggridR) (use a value from column `Res`). Unit: `integer`. Default: `5`.

### Most common errors
*More testing required*

### Null or error handling
*More testing required*