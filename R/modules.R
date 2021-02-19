#==============================================================================#
# modules.R

# This script contains reusable Shiny module functions that are the driving 
# force for this application. Each core module includes a 'server' function and 
# a 'ui' function used in the server.R and ui.R scripts, respectively to 
# generate the content and appearance of each component. There are also several
# helper modules 
#==============================================================================#

# Data Modules -----------------------------------------------------------------

#Function to dynamically filter data for the map
filter_data <- function(input, output, session, tab){
  ns <- session$ns
  
  reactive({
    if(tab == "Map"){
      data <- crime[date %within% interval(start = ymd(input$date[1]),
                                           end = ymd(input$date[2])),
      ][`crime description` %chin% input$description]
      
    } else{
      data <- crime[community %in% input$communities, ]
    }
    
    out <- data[, `:=`(
      label = sprintf(
        '<strong>Category:</strong> %s<br/><strong>Description:</strong> %s<br/><strong>Community:</strong> %s<br/><strong>Neighborhood:</strong> %s<br/><strong>Date:</strong> %s<br/><strong>Time:</strong> %s',
        `crime category`, `crime description`, community, neighborhood, date, time))]
    
    out$label <- lapply(out$label, htmltools::HTML)
    return(out)
  })
}

summary_data <- function(input, output, session){
  ns <- session$ns
  
  reactive({
    req(input$base_map_groups)
    community_filt <- when(
      input$communities,
      . == "All Communities" ~ crime[, unique(community)],
      . != "All Communities" ~ .)
    
    if("Communities" %in% input$base_map_groups){
      map_data <- crime_summary[year %in% input$yearfilter,
      ][, .(total = sum(N)), by = .(community)
      ][communities, on = "community"
      ][, `:=`(
        name = community,
        bins = cut(
          total, 
          leaflet:::getBins(
            domain = total, 
            bins = 5, 
            pretty = T
          )
        )
      )
      ][, `:=`(layerid = paste0(name, "_community"))]
      
      colors <- map_data[, .(
        bins = unique(bins), 
        color = t3(length(unique(bins))))]
      
      map_data <- map_data[colors, on = 'bins']
    }
    
    else if("Neighborhoods" %in% input$base_map_groups){
      map_data <- crime_summary[year %in% input$yearfilter,
      ][, .(total = sum(N)), by = .(neighborhood)
      ][neighborhoods, on = 'neighborhood'
      ][, `:=`(name = neighborhood, 
               bins = cut(total, leaflet:::getBins(domain = total, bins = 5, pretty = T)))
      ][, `:=`(layerid = paste0(name, "_neighborhood"))]
      
      colors <- map_data[, .(bins = unique(bins), color = t3(length(unique(bins))))]
      map_data <- map_data[colors, on = 'bins']
    }
    
    out <- map_data[, `:=`(
      label = sprintf("<strong>%s</strong><br/>Number of crimes: %g",
                      name, total))]
    
    out$label <- lapply(out$label, htmltools::HTML)
    return(out)
    
  })
}

# Input Menu -------------------------------------------------------------------

input_menu_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(
      type = 'text/css', 
      '#Inputs-input_menu {margin: 0% 2.5% 0% 2.5%}'),
    menuItemOutput(
      outputId = ns('input_menu')
    )
  )
}

input_menu <- function(input, output, session, tab){
  ns <- session$ns
  
  menu <- tagList(
    date = 
      div(id = "div_date",
          dateRangeInput(
            inputId = ns('date'),
            label = 'Dates',
            startview = 'year', 
            start = max(crime$date) - 1,
            end = max(crime$date)
          )
      ),
    
    description =
      div(id = "div_description",
          pickerInput(
            inputId = ns('description'),
            label = 'Crime Description',
            choices = crime_desc,
            multiple = T,
            selected = unlist(crime_desc, use.names = F),
            options = pickerOptions(
              liveSearch = T,
              actionsBox = T
            ))
      ),
    
    boundaries = radioGroupButtons(
      inputId = ns('boundaries'),
      label = 'Show Administrative Boundaries',
      selected = "Communities",
      choices = c('Communities', 'Neighborhoods'),
      direction = 'vertical'),
    
    communities = shinyjs::hidden(
      div(id = "div_communities",
          pickerInput(
            inputId = ns('communities'),
            label = 'Community',
            choices = c('All Communities', crime[, sort(unique(community))])
          ))),
    
    today = shinyjs::hidden(
      div(id="div_today",
          prettyCheckbox(
            inputId = ns('today'),
            label = 'Highlight Crimes in Last Day',
            value = FALSE,
            status = 'danger',
            shape = 'curve')
      )),
    
    timeunits = 
      div(id="div_timeunits", 
          pickerInput(
            inputId = ns('timeunit'),
            label = 'Time Unit',
            choices = c('hour', 'day', 'month', 'year')
          )),
    
    yearfilter = shinyjs::hidden(
      div(id = "div_yearfilter",
          pickerInput(
            inputId = ns('yearfilter'),
            label = 'Year',
            choices = crime[, unique(year)],
            multiple = T,
            selected = crime[, max(year)],
            options = pickerOptions(
              actionsBox = T)
          )
      ))
  )
  
  tablist <- list(
    `Community Profiles` = c('yearfilter'),
    `Crime Statistics` = c('date', 'description', 'timeunits'))
  
  tabs <- names(tablist)
  
  output$input_menu <- renderUI({
    menuItem(tabName = "Map", selected = T,
             {menu[c('date', 'yearfilter', 'description', 'timeunits')]}
    )
  })
  
  observeEvent(tab(), ignoreNULL = F, {
    to_hide <- tablist[[tabs[tabs != tab()]]]
    to_show <- tablist[[tabs[tabs == tab()]]]
    
    for(x in to_hide){
      shinyjs::hide(id = paste0('div_', x), asis = T)
    }    
    for(x in to_show){
      shinyjs::show(id = paste0('div_', x), asis = T)
    }
  })
  
}

# Info Module ------------------------------------------------------------------

info_ui <- function(id){
  ns <- NS(id)
  
  uiOutput(outputId = ns('info_text'))
  
}

info <- function(input, output, session){
  ns <- session$ns
  
  output$info_text <- renderUI({
    
    tagList(
      div(
        class = 'sidebartext',
        style = "white-space: normal; text-align: left;",
        h5("Use the input menu above to change the plots, tables, and map. The available inputs change depending on the selected tab:"),
        h5("Select the", strong("Crime Statistics"), "tab to visualize crime trends for specific crime categories and periods of time. Available inputs include:",
           tags$li(strong("Dates:"), "filters crimes that occur in a certain interval;"),
           tags$li(strong("Crime Description:"), "changes which crimes will appear on map; and"),
           tags$li(strong("Time Unit:"), "changes the time unit for the bar chart.")),
        h5("Select the", strong("Community Profiles"), "tab to see crime trends in specific communities or neighborhoods.
           To change the geographic area of interest, click the desired area on the map. Click the selected area again to return to a city-wide summary of the data.
           Available inputs include:",
           tags$li(strong("Year:"), "Changes the years visible in the summary table.")
        ),
        br(),
        h5("Download the data used in this dashboard:"),
        downloadHandler(
          filename = 'data.csv', content = function(file) {
            fwrite(crime, file)
          })
      ))
  })
}

# Map --------------------------------------------------------------------------

# Function to generate the layout of the map page, with boxes for the map
# output and the data shown on the map.
map_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(type = 'text/css', '.col-sm-12 {width: 97%}'),
    column(width = 7,
           fluidRow(
             box(width = 12,
                 title = "Change in crime from previous year", solidHeader = T,
                 valueBoxOutput(outputId = ns('summarybox1'), width = 6),
                 valueBoxOutput(outputId = ns('summarybox2'), width = 6))),
           fluidRow(
             box(id = 'mapbox',
                 width = 12,
                 leafletOutput(outputId = ns('base_map'))))),
    
    column(width = 5,
           tabBox(id = 'databox',
                  width = 12,
                  selected = 'Crime Statistics',
                  
                  tabPanel(
                    title = "Crime Statistics",
                    fluidRow(
                      box(id = 'topn', width = 12,
                          plotOutput(outputId = ns('top_n_plot')))
                    ),
                    fluidRow(
                      box(id = 'mosaic', width = 12,
                          plotOutput(outputId = ns('mosaic_plot'))
                      )
                    )
                  ),
                  tabPanel(
                    title = "Community Profiles",
                    fluidRow(
                      box(id = 'plot1', width = 12, title = "Plot",
                          plotOutput(outputId = ns('map_plot')))),
                    fluidRow(
                      box(id = 'data1', width = 12, title = 'Data',
                          div(reactable::reactableOutput(outputId = ns('map_data')))))
                  )
           ))
  )}

# Function to generate the place outputs in the layout generated by map_ui.
# renders a base map to which the filtered data is added using the map_add
# module and the map data table.
map <- function(input, output, session, filtered_data, polys){
  ns <- session$ns
  
  output$summarybox1 <- renderValueBox({
    valueBox(value = crime_change$pct_change[1], 
             subtitle = crime_change$violent[1],
             icon = icon(crime_change$symbol[1]),
             color = crime_change$color[1])
  })
  
  output$summarybox2 <- renderValueBox({
    valueBox(value = crime_change$pct_change[2], 
             subtitle = crime_change$violent[2],
             icon = icon(crime_change$symbol[2]),
             color = crime_change$color[2])
  })
  
  output$base_map <- renderLeaflet({
    leaflet() %>%
      fitBounds(
        lng1 = crime[, min(x)],
        lng2 = crime[, max(x)],
        lat1 = crime[, min(y)],
        lat2 = crime[, max(y)]) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(
          updateWhenIdle = T, 
          detectRetina = T, 
          updateWhenZooming = F, 
          preferCanvas = T)) %>%
      addPolygons(
        data = geo[type == "community",], 
        layerId = ~layerid, 
        group = "Communities", 
        weight = 1, 
        color = 'grey', 
        label = ~name,
        highlight = highlightOptions(
          weight = 5,
          color = "white")) %>%
      addPolygons(
        data = geo[type == 'neighborhood',], 
        layerId = ~layerid, 
        group = "Neighborhoods", 
        weight = 1, 
        color = 'grey', 
        label = ~name,
        highlight = highlightOptions(
          weight = 5,
          color = "white")) %>%
      addLayersControl(
        baseGroups = c("Communities", "Neighborhoods"), 
        options = layersControlOptions(collapsed = F))
  })
  
  output$map_data <- reactable::renderReactable({
    shiny::validate(
      need({input$yearfilter}, 
           message = "Wating on year input"))
    summary_table(
      .data = crime, 
      .year = input$yearfilter, 
      .community = polys$comms())
  })
  
  output$map_plot <- renderPlot({
    monthly_plot(
      .data = crime, 
      .year = input$yearfilter, 
      .community = polys$curr(), 
      .community_filt = polys$comms(), 
      .areaunit = polys$areaunit())
  })
  
  output$mosaic_plot <- renderPlot({
    req(input$timeunit)
    mosaic_plot(crime, input$timeunit)
  })
  
  # output$heatmap_plot <- renderPlot({
  #   heatcal_plot(dates = crime$date, xvar = 'week', yvar = 'day')
  # })
  
  output$top_n_plot <- renderPlot({
    top_n_plot(
      data = crime, 
      dates = c(input$date[1], input$date[2]), 
      n = 10, 
      grouping = 'crime description')
  })
  
  # output$ridge_plot <- renderPlot({
  #   req(input$communities, input$timeunit)
  #   ridgeline_plot(crime, input$timeunit, input$communities)
  # })
}

map_add <- function(input, output, session, proxy, filtered_data, summarized_data, polys, tab){
  
  ns <- session$ns
  
  observeEvent(tab(), {
    if (tab() == "Crime Statistics") {
      proxy %>%
        hideGroup('summary_polygons') %>%
        showGroup('filtered_points')
    }
    
    if (tab() == "Community Profiles") {
      proxy %>%
        hideGroup('filtered_points') %>%
        showGroup('summary_polygons')
    }})
  
  observeEvent(filtered_data(), {   
    if(nrow(filtered_data()) > 0) {
      
      if (tab() == "Crime Statistics") {
        proxy %>%
          clearMarkers() %>%
          addMarkers(
            data = filtered_data(),
            ~ x,
            ~ y,
            icon = ~crimeIcons[`crime category`],
            label = ~label, 
            group = "filtered_points"
          )
      }}
  })
  
  observeEvent(summarized_data(), {
    if (!is.null(summarized_data())) {
      
      proxy %>%
        setShapeStyle(
          map = ., 
          data = summarized_data(),
          layerId = ~layerid, 
          fillColor = ~color, 
          fillOpacity = 0.2,
          label = ~label
        )
    }
  })
  
  observeEvent(input$base_map_groups, {
    proxy %>%
      removeShape(layerId = c(polys$curr(), polys$prev())) %>%
      addPolygons(
        data = summarized_data()[name %in% c(polys$prev(), polys$curr())],
        layerId = ~name,
        fillColor = ~color,
        fillOpacity = 0.2,
        color = "grey",
        weight = 0.5,
        opacity = 1,
        highlight = highlightOptions(
          weight = 5,
          color = "white",
          sendToBack = T)) 
  })
  
  observeEvent(input$base_map_shape_click, {
    
    if (tab() == "Community Profiles") {
      print(polys$curr())
      print(polys$prev())
      proxy %>%
        removeShape(layerId = c(polys$curr(), polys$prev())) %>%
        addPolygons(
          data = summarized_data()[name == polys$prev()],
          layerId = ~name,
          color = "grey",
          weight = 0.5,
          opacity = 1,
          fillOpacity = 0,
          highlight = highlightOptions(
            weight = 5,
            color = "white")) %>%
        addPolygons(
          data = summarized_data()[name == polys$curr()],
          layerId = ~name,
          weight = 5, 
          opacity = 1, 
          fillOpacity = 0,
          color = "#A36888")
    }
  })
}

selected_poly <- function(input, output, session) {
  ns <- session$ns
  allcomm <- unique(crime$community)
  
  poly <- reactiveValues(
    prev = c('All Communities'),
    curr = c('All Communities'),
    comms = allcomm,
    areaunit = c('community')
  )
  
  observeEvent(input$base_map_shape_click, {
    p <- input$base_map_shape_click
    id <- str_remove(p$id, "\\_\\w{1,}$")
    
    poly$prev <- poly$curr
    
    if(id != poly$curr){
      poly$curr <- id
      poly$comms <- poly$curr
      poly$areaunit <- c("neighborhood")
    }
    else {
      poly$curr <- c("All Communities")
      poly$comms <- allcomm
      poly$areaunit <- c("community")
    }
  })
  
  observeEvent(input$databox, {
    poly$prev <- c("All Communities")
    poly$curr <- c("All Communities")
    poly$comms <- allcom
    poly$areaunit <- c("community")
  })
  
  out <- list(
    prev = reactive({poly$prev}),
    curr = reactive({poly$curr}),
    comms = reactive({poly$comms}),
    areaunit = reactive({poly$areaunit})
  )
  return(out)
}
