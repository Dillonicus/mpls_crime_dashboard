################################################################################

################################################################################

# Function to clean and perform geocoding of crime data

crime_clean <- function(.data, .neighborhoods, .communities){
  unique_intersect <- function(communities, neighborhoods) {
    intersects <- st_intersects(communities[['geometry']], neighborhoods[['geometry']])
    out <- map_dfr(1:length(intersects),
                   ~{
                     data.table(community = communities$name[.x], 
                                neighborhood = neighborhoods$name[intersects[[.x]]],
                                area = st_area(st_intersection(communities$geometry[.x], neighborhoods$geometry[intersects[[.x]]])))
                   })
    
    out[, .(community, area, .N), by = .(neighborhood)
    ][, N := ifelse(N > 1, 1, 0)
    ][order(neighborhood, N, -area)
    ][, .SD[1], by = .(neighborhood, N)
    ][, .(community, neighborhood)]
    
  }
  
  comm_geo <- .communities[['geometry']]
  neighb_geo <- .neighborhoods[['geometry']]
  bbox <- st_bbox(comm_geo)
  
  dat <- .data[between(x, bbox$xmin, bbox$xmax) & between(y, bbox$ymin, bbox$ymax),][, `:=`(
    col.id = .I,
    date  = date(reporteddatetime),
    time  = strftime(x = reporteddatetime,
                     format = "%H:%M:%S %p"),
    year  = lubridate::year(reporteddatetime),
    week  = week(reporteddatetime),
    day   = lubridate::wday(reporteddatetime,
                            label = T),
    month = lubridate::month(reporteddatetime,
                             label = T),
    hour  = strftime(reporteddatetime,
                     format = "%H:00"))][,
                                         today := if_else(Sys.Date() - date < 2,
                                                          true = 1,
                                                          false = 0)]
  
  dat <- codes[dat, on = "offense"]
  
  geo <- st_as_sf(dat[, .(x,y)], coords = c('x', 'y'), crs = st_crs(comm_geo))
  
  neighborhood_intersect <- st_intersects(neighb_geo, geo)
  neighborhood_match <- map_dfr(.x = 1:length(neighborhood_intersect), .f = ~data.table(neighborhood = .neighborhoods$name[[.x]], col.id = neighborhood_intersect[[.x]]))
  
  community_match <- unique_intersect(communities, neighborhoods)
  
  match <- community_match[neighborhood_match, on = "neighborhood"][, .(col.id, community, neighborhood)]
  
  match[dat, on = 'col.id'][!is.na(community) & community != "", -c('col.id')]
}

# Shapefiles for Minneapolis Communities and Neighborhoods

communities <- data.table(st_read('data/communities/Communities.shp', layer = 'Communities'))
communities <- communities[, .(name = CommName, geometry)][, `:=`(layerid = paste(name, 'community', sep = "_"))]
neighborhoods <- data.table(st_read('data/neighborhoods/Neighborhoods.shp', layer = 'Neighborhoods'))
neighborhoods <- neighborhoods[, .(name = BDNAME, geometry)][, `:=`(layerid = paste(name, 'neighborhood', sep = "_"))]

geo <- rbind(communities[, .(name, type = 'community', geometry, layerid)], neighborhoods[, .(name, type = 'neighborhood', geometry, layerid)])

geolist <- list(
  community = communities,
  neighborhood = neighborhoods
)


# File with UCR codes, crime category, and offense codes to translate into readable
# crime descriptions. Merged with the final crime dataset
codes <- fread("data/UCR.txt")

# # Import police incidents data located on the Open Minneapolis portal
# pims_raw1 <- list(
#    yr2010 = "https://opendata.arcgis.com/datasets/6d08a19ce96b42ab844bcf08c70e5480_0.csv",
#    yr2011 = "https://opendata.arcgis.com/datasets/657ebd6b1af14af296e1004ed02080d8_0.csv",
#    yr2012 = "https://opendata.arcgis.com/datasets/83508d169ebb48199273d21fef90fb30_0.csv",
#    yr2013 = "https://opendata.arcgis.com/datasets/944cbb45a5fd4f6dbee2f7136f166184_0.csv",
#    yr2014 = "https://opendata.arcgis.com/datasets/f0279f3673394c66a96c03e6e42287f4_0.csv",
#    yr2015 = "https://opendata.arcgis.com/datasets/08ff2c3bec594dd2a7a8566b2a81d452_0.csv",
#    yr2016 = "https://opendata.arcgis.com/datasets/0b12e290edb64816a7cd5270fdd6bacb_0.csv",
#    yr2017 = "https://opendata.arcgis.com/datasets/3d33a4f94a004fb5816936708642e045_0.csv",
#    yr2018_1 = "https://opendata.arcgis.com/datasets/58e6f399e0f04c568b3ba45086d15818_0.csv")
# 
# # Minneapolis switched to a new reporting system in June 2018 with slightly
# # different data structure. Data from this point onward is processed differently
# pims_raw2 <- list(
#    yr2018_2 = "https://opendata.arcgis.com/datasets/055e662af18c4488b54dcbd496f897b7_0.csv",
#    yr2019 = "https://opendata.arcgis.com/datasets/8cd15449ac344aa5a55be7840d67c52d_0.csv",
#    yr2020 = "https://opendata.arcgis.com/datasets/35c7de976a60450bb894fc7aeb68aef6_0.csv")
# 
# # Download data and combine
# pims2010_2018 <- rbindlist(lapply(pims_raw1, fread))
# pims2018_2020 <- rbindlist(lapply(pims_raw2, fread))
# 
# # Change variables to lower case
# setnames(pims2010_2018, old = names(pims2010_2018), new = str_to_lower(names(pims2010_2018)))
# setnames(pims2018_2020, old = names(pims2018_2020), new = str_to_lower(names(pims2018_2020)))
# 
# # Select appropriate variables and combine into one data set
# crime2010_2020 <- rbind(
#    pims2010_2018[, .(precinct, reporteddatetime = reporteddate, offense, description, y = lat, x = long, casenumber = ccn)],
#    pims2018_2020[, .(precinct, reporteddatetime, offense, description, y, x, casenumber)]
# )
# 
# # Clean and geocode 2010-2019 data
# crime2010_2020 <- crime_clean(crime2010_2020, neighborhoods, communities)
# 
# # Write 2010-2019 data to a static .csv for use in Shiny app
# fwrite(crime2010_2020, "data/crime2010_2020.csv")

crime2010_2020 <- fread('data/crime2010_2020.csv')[, `:=`(date = date(date))]

# Read in 2021 data and clean
pims2021 <- fread('https://opendata.arcgis.com/datasets/cb6a8b1d01b74feea5d3f96fa79bb6bf_0.csv')
setnames(pims2021, names(pims2021), str_to_lower(names(pims2021)))
crime2021 <- pims2021[, .(precinct, reporteddatetime, offense, description, y, x, casenumber)]

crime2021 <- crime_clean(crime2021, neighborhoods, communities)

# Combine historical data with current year data
crime <- rbind(crime2010_2020, crime2021)[, violent := ifelse(`ucr code` %in% c(1,3,4,5), "Violent Crime", "Property Crime")]

# Summary data
# crime_summary <- crime[, .N, by = .(year, month, violent, `crime category`, community, neighborhood)]
crime_summary <- crime[, .N, by = .(year, month, violent, `crime category`, community, neighborhood)]
crime_summary <- melt(crime_summary, measure.vars = c('community', 'neighborhood'), variable.name = 'areaunit', value.name = 'name')

# Previous year
crime_change <- change_from(crime)