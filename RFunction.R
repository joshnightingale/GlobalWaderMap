#### hex1.R port ####

## TO DO
## Input of data (& format - move2)
## Input of user-selected resolution
## Output format (move2?)


#### packages ####
library(sf)
library(dggridR)
library(plyr)
library(mapview)


#### load shapefile ####
# full data
# dat_string <- "../../Movebank/Basic map/moveapps-shapefile/moveapps-shapefile.shp"

## just wadertrack for testing
dat_string <- "../../Movebank_experiments/moveapps-shapefilevgk and wadertrack/moveapps-shapefile.shp"

## read
dat <- st_read(dat_string)


## set gridsize
# roughly: 5 for testing; 6 for embed; 7 for hi-res
gridsize <- 5

# rFunction = function(dat, sdk, gridsize=5, ...) {
  #### world map ####
  
  ### load saved file
  world <- st_read("world.gpkg")
  
  ### how to generate and save (once)
  # world <- ne_countries(scale = "medium", returnclass = "sf")
  # write_sf(world, "world.gpkg")
  
  
  #### make grid ####
  
  ## raw grid
  # see https://github.com/r-barnes/dggridR for grid sizes etc.
  # hex <- dgconstruct(res = 5) # huge cells for testing (~1500 km !)
  # hex <- dgconstruct(res = 7) # about right for main view (~100 km)
  hex <- dgconstruct(res = gridsize) # for embed
  
  
  ## create cells covering shapefile
  ## TODO - add code to save as shapefile
  
  # using shapefile, res5 and wadertrack - 11.94468 secs
  # using res7 and full data = 4.5 minutes
  cells <- dgshptogrid(hex, dat_string)
  
  
  # fix edge of world wrapping
  wrap_cells <- st_wrap_dateline(cells, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  
  
  ### #Get the corresponding grid cells for each point
  
  # with res 5 and wadertrack = 0.09630871 secs
  # with res 7 and full data, 5.65 secs
  dat$seqnum <- dgGEO_to_SEQNUM(hex, dat$loc_long, dat$loc_lat)$seqnum
  
  
  ### summarise by count
  hex_sum <- ddply(dat, .(seqnum), \(x) {
    data.frame(
      count=nrow(x),
      Nind = length(unique(x$anim_lo_id)),
      Nspp = length(unique(x$taxon))
    ) |> return
  }, .progress = progress_time())
  
  summary(hex_sum)
  
  # add counts to cells
  # wrap_cells$count <- NA
  wrap_cells$count <- hex_sum$count[match(wrap_cells$seqnum, hex_sum$seqnum)]
  wrap_cells$Nind <- hex_sum$Nind[match(wrap_cells$seqnum, hex_sum$seqnum)]
  wrap_cells$Nspp <- hex_sum$Nspp[match(wrap_cells$seqnum, hex_sum$seqnum)]
  
  summary(wrap_cells)
  
  # remove empty cells for faster loading
  gw_map <- na.omit(wrap_cells)
  
  # write out shapefile
  # st_write(wrap_cells, paste0("output/wrap_nona_res6full-", Sys.time(), ".shp"),
  #          driver = "ESRI Shapefile")
  
  
  
  
  
  #### plot with Mapview ####
  
  ### each layer in separate mapview() function call,
  ### then add them together!
  
  # TODO: use this for breaks  (evenly spaced on log scale)
  # pretty(log(gw_map$Nspp), n=7)|>exp()|>round()|>unique()
  
  # setup & locations
  widget <- mapview(gw_map, # R object
                    zcol="count", # column -> layer
                    layer.name="Daily locations",
                    # at=10^(0:6), # breakpoints for scale
                    at=(10^(pretty(log10(gw_map$count), n=7))|>round()|>unique()),
                    color=NULL, # polygon borders
                    map.types=c("OpenStreetMap", "Esri.WorldImagery")) + # basemaps
    
    # individuals
    mapview(gw_map, zcol="Nind", color=NULL, layer.name="Individuals",
            at=(pretty(log(gw_map$Nind), n=7)|>exp()|>round()|>unique()),
            hide=T) + # don't show initially
    
    # species
    mapview(gw_map, zcol="Nspp", color=NULL, hide=T, layer.name="Species",
            at=(pretty(log(gw_map$Nspp), n=7)|>exp()|>round()|>unique()))
  
  
  mapshot(widget, url = appArtifactPath(paste0("map", gridsize, ".html")))
  
  return(gw_map)
  
# }