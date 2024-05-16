#### hex1.R port ####

## TO DO
## Input of data (& format - move2)
## Input of user-selected resolution
## Output format (move2?)


#### packages ####
library(plyr)
library(move2)
library(magrittr)
library(sf)
library(dggridR)
library(mapview)


rFunction = function(gridsize, data) {
  
  #### load saved world map ####
  world <- st_read(getAuxiliaryFilePath("map"))
  
  
  #### make grid system ####
  # see https://github.com/r-barnes/dggridR for grid sizes etc.
  hex <- dgconstruct(res = gridsize) 
  
  
  #### return hex cells ####
  
  ## needs shapefile of data extent
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
  
  cat(str( subset(data, seqnum==data$seqnum[1])  ))
  
  #### summarise cell contents ####
  # hex_sum <- ddply(data, .(seqnum), \(x) {
  #   data.frame(
  #     count=nrow(x),
  #     Nind = length(unique(x$track_id))#,
  #     # Nspp = length(unique(x$taxon_canonical_name))
  #   ) %>% return()
  # })
  
  
  hex_sum <- data.frame(seqnum=unique(data$seqnum), 
                        count=NA, Nind=NA, Nspp=NA)
  for (ii in 1:nrow(hex_sum) ) {
    data_sub <- subset(data, seqnum==hex_sum$seqnum[ii])
    hex_sum$count[ii] <- nrow(data_sub)
    hex_sum$Nind[ii] <- length(unique(attr(data_sub, "track_data")$individual_id))
    hex_sum$Nspp[ii] <- length(unique(attr(data_sub, "track_data")$taxon_canonical_name))
  }
  
  
  cat(str(hex_sum))
  # cat(str(wrap_cells))
  
  # add counts to cells
  wrap_cells$count <- hex_sum$count[match(wrap_cells$seqnum, hex_sum$seqnum)]
  wrap_cells$Nind <- hex_sum$Nind[match(wrap_cells$seqnum, hex_sum$seqnum)]
  wrap_cells$Nspp <- hex_sum$Nspp[match(wrap_cells$seqnum, hex_sum$seqnum)]
  
  
  
  
  ## remove empty cells for faster loading
  wrap_cells %<>% na.omit
  
    #### plot with Mapview ####
  
  ### each layer needs separate mapview() function call, then add
  
  # setup & locations
  widget <- mapview(wrap_cells, # R object
                    zcol="count", # column -> layer
                    layer.name="Daily locations",
                    # # breakpoints for scale
                    at=(10^(pretty(log10(wrap_cells$count), n=7)) |> round() |>unique()),
                    color="#FFFFFF00", # polygon borders
                    map.types=c("OpenStreetMap", "Esri.WorldImagery") # basemaps
                    ) + 

    # individuals
    mapview(wrap_cells, zcol="Nind", color="#FFFFFF00", layer.name="Individuals",
            at=(pretty(log(wrap_cells$Nind), n=7) |> exp() |> round() |> unique()),
            hide=T) + # don't show initially

    # species
    mapview(wrap_cells, zcol="Nspp", color="#FFFFFF00", hide=T, layer.name="Species",
            at=(pretty(log(wrap_cells$Nspp), n=7) |> exp() |> round() |> unique()))
  
  
  # save html widget of map as artefact
  mapshot(widget, url = appArtifactPath("map.html"))
  

  # output 
  return(data)
  
}