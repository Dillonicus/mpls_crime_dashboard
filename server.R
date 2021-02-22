################################################################################
# server.R

# This script executes the modules contained in the modules.R file to construct
# the components of the Shiny App. 
################################################################################

shinyServer(function(input, output, session) {

  poly <- callModule(module = selected_poly, id = "Inputs")
  
  ##### Inputs Menu ------------------------------------------------------------
  
  # Get currently selected tab
  selected_tabbox <- reactive({input$databox})
  
  # Generate input menu based on currently selected tab
  callModule(input_menu, id = "Inputs", tab = selected_tabbox)
  
  callModule(info, id = "Info")
  ##### Map --------------------------------------------------------------------
  
  # Generate filtered data to display on map based on inputs
  map_data <- callModule(filter_data, id = "Inputs", tab = "Map")
  #summarized_data <- callModule(module = summary_data, id = "Inputs")
  summarized_data <- callModule(module = summary_data, id = "Inputs", polys = poly)
  
  # Generate base map
  callModule(map, id = "Inputs", filtered_data = map_data, polys = poly)
  
  
  # Create proxy object to add points to based on filtered data
  proxy <- leafletProxy("Inputs-base_map")
  
  # Add objects to map proxy
  callModule(
    map_add, 
    id = "Inputs", 
    proxy = proxy, 
    filtered_data = map_data, 
    summarized_data = summarized_data, 
    polys = poly, 
    tab = selected_tabbox
  )
  }
  )

