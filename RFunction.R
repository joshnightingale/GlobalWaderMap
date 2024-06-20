#### packages ####
library(move2)
library(magrittr)
library(sf)
library(dggridR)
library(mapview)
library(webshot)


rFunction = function(gridsize=5, data) {
  
  #### make grid system ####
  # see https://github.com/r-barnes/dggridR for grid sizes etc.
  hex <- dgconstruct(res = gridsize) 
  
  
  #### return hex cells ####
  
  ## dgshptogrid() needs shapefile of data extent
  bbox_shp <- st_bbox(data) %>% st_as_sfc() # return
  st_write(obj = bbox_shp, dsn = Sys.getenv(x = "APP_ARTIFACTS_DIR"), 
           layer = "bbox.shp", driver = "ESRI Shapefile", append=F) # save
  
  
  ## return cells
  cells <- dgshptogrid(hex, appArtifactPath("bbox.shp"))
  
  
  ## fix edge of world wrapping
  wrap_cells <- st_wrap_dateline(cells, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  
  
  ## Get the corresponding grid cells for each point
  coords <- st_coordinates(data)
  data$seqnum <- dgGEO_to_SEQNUM(hex, coords[,1], coords[,2])$seqnum
  
  
  
  #### summarise cell contents ####
  
  ## empty data frame for results
  hex_sum <- data.frame(seqnum=unique(data$seqnum), 
                        count=NA, Nind=NA, Nspp=NA)
  
  
  ## loop through each cell (seqnum) & extract summaries
  for (ii in 1:nrow(hex_sum) ) {
    data_sub <- subset(data, seqnum==hex_sum$seqnum[ii])
    hex_sum$count[ii] <- nrow(data_sub)
    hex_sum$Nind[ii] <- length(unique(attr(data_sub, "track_data")$individual_id))
    hex_sum$Nspp[ii] <- length(unique(attr(data_sub, "track_data")$taxon_canonical_name))
  }
  
  
  ## add counts to cells
  wrap_cells$Locations <- hex_sum$count[match(wrap_cells$seqnum, hex_sum$seqnum)]
  wrap_cells$Individuals <- hex_sum$Nind[match(wrap_cells$seqnum, hex_sum$seqnum)]
  wrap_cells$Species <- hex_sum$Nspp[match(wrap_cells$seqnum, hex_sum$seqnum)]
  
  
  ## remove empty cells for faster loading
  wrap_cells %<>% na.omit
  
  
  
  #### plot with Mapview ####
  
  ## each layer needs separate mapview() function call, added together
  
  # setup & locations
  widget <- (mapview(wrap_cells, # R object
                     zcol="Locations", # column -> layer
                     layer.name="Daily locations",
                     # # breakpoints for scale
                     at=(10^(pretty(log10(wrap_cells$Locations), n=7)) |> round() |>unique()),
                     color="#FFFFFF00", # polygon borders
                     popup = leafpop::popupTable(wrap_cells, # popup attribute table
                                                 zcol=c("Locations", "Individuals", "Species"),
                                                 row.numbers = FALSE, feature.id = FALSE),
                     map.types=c("OpenStreetMap", "Esri.WorldImagery"), # basemap
                     hide=F) + # hide all layers but the first
    
    # individuals
    mapview(wrap_cells, zcol="Individuals", color="#FFFFFF00", layer.name="Individuals",
            at=(pretty(log(wrap_cells$Individuals), n=7) |> exp() |> round() |> unique()),
            popup = leafpop::popupTable(wrap_cells, # popup attribute table
                                        zcol=c("Locations", "Individuals", "Species"),
                                        row.numbers = FALSE, feature.id = FALSE),
            hide=T) + # don't show initially
    
    # species
    mapview(wrap_cells, zcol="Species", color="#FFFFFF00", layer.name="Species",
            at=(pretty(log(wrap_cells$Species), n=7) |> exp() |> round() |> unique()), 
            popup = leafpop::popupTable(wrap_cells, # popup attribute table
                                        zcol=c("Locations", "Individuals", "Species"),
                                        row.numbers = FALSE, feature.id = FALSE),
            hide=T)) %>% removeMapJunk(junk = "homeButton") 
  
  
  # save html widget of map as artefact
  mapshot(widget, url = appArtifactPath("map.html"),
          selfcontained = TRUE)
  

  # remove directory created by mapshot/webshot
  unlink(appArtifactPath("map_files"), recursive = T)
  
  
  ### TODO this should use appArtificatrPath
  # remove shapefile
  paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR"), grep("bbox", list.files(Sys.getenv(x = "APP_ARTIFACTS_DIR")) , 
         value = T)) %>% unlink
  
  
  # output 
  return(data)
  
}
