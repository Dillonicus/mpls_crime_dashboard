#==============================================================================#
# global.R
#
# This script imports, cleans, and geocodes police incidents data from the
# Open Minneapolis data portal (https://opendata.minneapolismn.gov) using 
# shape file data for Minneapolis communities and neighborhoods.
#==============================================================================#

# Libraries --------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(glue)
library(leaflet)
library(dplyr)
library(lubridate)
library(plotly)
library(forcats)
library(ggplot2)
library(scales)
library(ggridges)
library(stringr)
library(purrr)
library(data.table)
library(rintrojs)
library(shinyjs)
library(sf)
library(reactable)

# Source Files -----------------------------------------------------------------

files <- c("R/functions.R", "R/data_clean.R", "R/functions.R", "R/modules.R")

lapply(files, function(x) invisible(source(file = x)))

# Functions --------------------------------------------------------------------

html_dependency_prism <- function() {
   htmltools::htmlDependency(
      "prism",
      "1.4.1",
      src = 'www/prism/',
      stylesheet = "prism.css",
      script = "prism.js"
   )
}

prismAddTags <- function(code, language = "r") {
   paste0("<pre><code class = 'language-", language, "'>",
          code, 
          "</code></pre>")
}
prismCodeBlock <- function(code, language = "r") {
   tagList(
      HTML(prismAddTags(code, language = language)),
      tags$script("Prism.highlightAll()")
   )
}

leafletjs <- tags$head(
   # add in methods from https://github.com/rstudio/leaflet/pull/598
   tags$script(HTML(
      "
window.LeafletWidget.methods.setStyle = function(category, layerId, style, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === 'object' && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);
  //console.log(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.setStyle(style[i]);
    }
  });
  
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      // layer.setStyle(style[i]);
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
"
   )))

# Leaflet functions ------------------------------------------------------------

# Clean data.table objects to be passed to leaflet directly
sanitize_dt <- function(obj){
   sanitized_dt <- obj[["geometry"]]
   attr(sanitized_dt, "names") <- NULL
   sanitized_dt
}

# Adds sanitize_dt to leaflet namespace
environment(sanitize_dt) <- asNamespace("leaflet")

# Function to add polygons from data.table
polygonData.data.table <- function(obj) {
   leaflet:::polygonData(sanitize_dt(obj))
}

# Add polygonData.data.table to leaflet namespace
environment(polygonData.data.table) <- asNamespace("leaflet")

# Crime categories/descriptions
crime_desc <- purrr::map(unique(codes$`crime category`),
           ~{
              a <- list(codes[`crime category` == .x, `crime description`])
              names(a) <- .x
              a
           }) %>% flatten()
