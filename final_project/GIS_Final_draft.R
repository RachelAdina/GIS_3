library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(spData)
library(glue)
library(readr)

library(tmap)
library(leaflet)
library(mapview)
library(ggplot2)

citation('tidycensus')
citation('spData')

setwd("C:/Users/rache/Documents/GitHub/GIS_3/final_project")


# STEP 1: COUNTY SHAPEFILES

#census_api_key("aeb3a2d13263357ddb5af381a90ce369a3e1a85f", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")
options(tigris_class = "sf")
options(tigris_use_cashe = TRUE)

#get shapefile from 2014-2018 5-year aCS
#counties <- get_acs(geography = 'county',
#                    variables = 'B01001_001',
#                    geometry = TRUE)

counties = counties %>%
  select(GEOID, NAME, geometry) %>%
  filter(!grepl('Puerto Rico', NAME)) %>%
  filter(!grepl('Alaska', NAME)) %>%
  filter(!grepl('Hawaii', NAME))


data(us_states)
crosswalk <- us_states %>% 
  st_drop_geometry() %>%
  select(GEOID, NAME, REGION) %>%
  rename(st_fips = GEOID)

counties = counties %>% separate(NAME, into = c('county', 'state'), sep=', ')
counties = left_join(counties, crosswalk, by=c('state'='NAME'))

#counties = st_transform(counties, 2163)

#st_write(counties, 'counties.shp')
#counties <- st_read('counties.shp')

#i <- sapply(counties, is.factor)
#counties[i] <- lapply(counties[i], as.character)



# STEP 2: MIGRATION DATA

directions = c('in', 'out')
years = c('1314', '1415', '1516', '1617', '1718')

#url_list <- list()
#for(year in years){
#  url = glue(irs_base)
#  url_list <- url
#}

#url_list


download_files = function(dir, year){
  url = glue('https://www.irs.gov/pub/irs-soi/county{dir}flow{years}.csv')
  filename = glue('{dir}flow{year}.csv')
  download.file(url, filename)
}


for(dir in directions){
  for(year in years){
    download_files(dir, year)
  }
}


load_inflows = function(name, yr){
  df = read_csv(name)
  df = subset(df, y2_countyfips!='000' & y1_statefips !='96')
  df['year'] <- yr
  df = df %>% 
    filter(grepl('Total Migration-US', y1_countyname)) %>%
    unite('geoid', y2_statefips:y2_countyfips, sep='', remove=TRUE) %>%
    select(geoid, year, n1) %>%
    rename(inflows = n1)
}



#inflow1718 = load_inflows('inflow1718.csv', '2018')


inflow_list <- list()
for(year in years){
  df = load_inflows(glue('inflow{year}.csv'), year)
  inflow_list[[year]] <- df
}

inflows = do.call('rbind', inflow_list)
substring(inflows$year, 1, 2) <- '20'



load_outflows = function(name, yr){
  df = read_csv(name)
  df = subset(df, y1_countyfips!='000' & y2_statefips !='96')
  df['year'] <- yr
  df = df %>% 
    filter(grepl('Total Migration-US', y2_countyname)) %>%
    unite('geoid', y1_statefips:y1_countyfips, sep='', remove=TRUE) %>%
    select(geoid, year, n1) %>%
    rename(outflows = n1)
}


#outflow1718 = load_outflows('outflow1718.csv', '2018')


outflow_list <- list()
for(year in years){
  df = load_outflows(glue('outflow{year}.csv'), year)
  outflow_list[[year]] <- df
}

outflows = do.call('rbind', outflow_list)
substring(outflows$year, 1, 2) <- '20'



# STEP 3: MERGE

net_join <- function(inflow, outflow, shape){
  df = inner_join(inflow, outflow, by=c('geoid', 'year'))
  df['net_migration'] = df$inflows - df$outflows
  shape_merge = left_join(shape, df, by=c('GEOID'='geoid'))
}


counties_merged = net_join(inflows, outflows, counties)


#county1314 = net_join(inflow1314, outflow1314, counties)
#county1415 = net_join(inflow1415, outflow1415, counties)
#county1516 = net_join(inflow1516, outflow1516, counties)
#county1617 = net_join(inflow1617, outflow1617, counties)
county1718 = net_join(inflow1718, outflow1718, counties)



# STEP 4: AGGREGATE 

aggregate_geom = function(df, col){
  level <- enquo(col)
  new_df = df %>% group_by((!!level), year) %>%
    summarize(net_migrants = sum(net_migration, na.rm=TRUE))
}


#state1718 = aggregate_geom(county1718, state)
#region1718 = aggregate_geom(county1718, REGION)

states_merged = aggregate_geom(counties_merged, state)
regions_merged = aggregate_geom(counties_merged, REGION)



plot(county1718['net_migration'])
plot(state1718['net_migrants'])
plot(region1718['net_migrants'])



# INTERACTIVE MAPPING

pal <- colorQuantile('RdYlBu', state1718$net_migrants, n=6)

test2 = leaflet(state_sp) %>% 
  addPolygons(fillColor = pal(state1718$net_migrants),
              weight=2,
              opacity=1, 
              color='black',
              fillOpacity=0.8,
              highlight = highlightOptions(
                weight = 4,
                color = 'white',
                fillOpacity=0.9,
                bringToFront=TRUE),
              label = state1718$net_migrants) %>%
  addLegend(pal = pal, values = ~net_migrants, opacity = 0.8, 
            title='Net Migration', position='bottomright')

test2


state18 <- filter(states_merged,
                  year == '2018')

test3 = leaflet(state18) %>% 
  addPolygons(fillColor = pal(state18$net_migrants),
              weight=2,
              opacity=1, 
              color='black',
              fillOpacity=0.8,
              highlight = highlightOptions(
                weight = 4,
                color = 'white',
                fillOpacity=0.9,
                bringToFront=TRUE),
              label = state18$net_migrants) %>%
  addLegend(pal = pal, values = ~net_migrants, opacity = 0.8, 
            title='Net Migration', position='bottomright')

test3





# STEP 5: OUTFLOWS

load_flowMapData <- function(file, col, yr){
  df <- read_csv(file)
  df['year'] = yr
  level = enquo(col)
  df = subset(df, y1_statefips!='00' & y1_statefips!='02' &
                y1_statefips!='15' & y2_statefips!="57" &
                y2_statefips!='58' & y2_statefips!='59' & 
                y2_statefips!='72' & y2_statefips!="96" & 
                y2_statefips!="98" & y2_statefips!="97" &
                y2_statefips!='02' & y2_statefips!='15')
  df = left_join(df, crosswalk, by=c('y1_statefips'='st_fips'))
  df = df %>% rename(origin = (!!level))
  df = left_join(df, crosswalk, by=c('y2_statefips'='st_fips'))
  df = df %>% rename(destination = (!!level))
  df = df %>%
    select('year', 'origin', 'destination', 'n1') %>%
    rename(migrants = n1) %>%
    group_by(year, origin, destination) %>%
    summarize(total_migrants = sum(migrants))
}

stateflow18 <- load_flowMapData('outflow1718.csv', NAME, '2018')
regionflow18 <- load_flowMapData('outflow1718.csv', REGION, 2018)



#calculate centroids
county_cent = st_centroid(counties)
state_cent = st_centroid(state1718)
region_cent = st_centroid(region1718)

cent_coords <- do.call(rbind, st_geometry(county_cent)) %>%
  as_tibble() %>% setNames(c('lon', 'lat'))

state_coords <- do.call(rbind, st_geometry(state_cent)) %>%
  as_tibble() %>% setNames(c('lon', 'lat'))

region_coords <- do.call(rbind, st_geometry(region_cent)) %>%
  as_tibble() %>% setNames(c('lon', 'lat'))

county_cent = merge(county_cent, cent_coords, by='row.names')
county_cent = county_cent %>% st_drop_geometry() %>%
  select(GEOID, lon, lat)

state_cent = merge(state_cent, state_coords, by='row.names')
state_cent = state_cent %>% st_drop_geometry() %>%
  select(state, lon, lat)

region_cent = merge(region_cent, region_coords, by='row.names')
region_cent = region_cent %>% st_drop_geometry() %>%
  select(REGION, lon, lat)


flows = left_join(outflow_17, county_cent, by=c('orig_id'='GEOID'))
flows = flows %>% rename(orig_lon = lon, orig_lat = lat)

flows = left_join(flows, county_cent, by=c('dest_id'='GEOID'))
flows = flows %>% rename(dest_lon = lon, dest_lat = lat)


state_flows = left_join(state_flows, state_cent, by=c('origin'='state'))
state_flows = state_flows %>% rename(orig_lon = lon, orig_lat = lat)

state_flows = left_join(state_flows, state_cent, by=c('destination'='state'))
state_flows = state_flows %>% rename(dest_lon = lon, dest_lat = lat)

reg_flows = left_join(regionflow18, region_cent, by=c('origin'='REGION'))
reg_flows = reg_flows %>% rename(orig_lon = lon, orig_lat = lat)

reg_flows = left_join(reg_flows, region_cent, by=c('destination'='REGION'))
reg_flows = reg_flows %>% rename(dest_lon = lon, dest_lat = lat)


library(geosphere)

test <- gcIntermediate(reg_flows[,5:6], reg_flows[,7:8], sp = TRUE, addStartEnd = TRUE)

test$counts <- reg_flows$total_migrants
test$origins <- reg_flows$origin
test$destinations <- reg_flows$destination





library(leaflet)
library(RColorBrewer)

hover <- paste0(test$origins, " to ", 
                test$destinations, ': ', 
                as.character(test$counts))

#pal <- colorFactor(brewer.pal(10, 'Set2'), flows$origins)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = test, weight = ~counts, label = hover, 
               group = ~origins) %>%
  addLayersControl(overlayGroups = unique(test$origins), 
                   options = layersControlOptions(collapsed = FALSE))




