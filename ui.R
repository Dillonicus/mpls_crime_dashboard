#==============================================================================#
# ui.R

# This script provides the framework for how the app appears. It includes a 
# header with a dropdown menu, side navigation bar, and the body of the app.
# It also includes a introduction/help tool based on JavaScript.
#==============================================================================#

# header <- dashboardHeaderPlus(
#   rightSidebarIcon = "sliders",
#   dropdownMenu(
#     type = "notifications", 
#     icon = icon("question"),
#     badgeStatus = NULL,
#     headerText = "",
#     notificationItem(
#       text = actionButton(
#         inputId = "help",
#         label = "Click For Help",
#         icon = icon("question")
#         ),
#       icon = icon(""), 
#       status = "primary")
#     )
#   )

header <- dashboardHeader()


sidebar <- dashboardSidebar({
  
  sidebarMenu(
    id = "tabs",
    menuItem(
      tabName = "Map",
      text = "Map",
      icon = icon("globe-americas"), 
      selected = T, 
      startExpanded = T),
    input_menu_ui("Inputs"),
    menuItem(
      tabName = "Code",
      text = "Code",
      icon = icon("code"),
      menuSubItem(text = "global.R", tabName = "global"),
      menuSubItem(text = "ui.R", tabName = "ui"),
      menuSubItem(text = "server.R", tabName = "server"),
      menuSubItem(text = "functions.R", tabName = "functions"),
      menuSubItem(text = "modules.R", tabName = "modules")
    ),
    menuItem(
      text = "Info", 
      icon = icon("question"), startExpanded = T,
      info_ui("Info")),
    useShinyjs())
})

body <- dashboardBody({
  
  tabItems(
    tabItem(
      tabName = "Map",
      map_ui(id = "Inputs")
    ),
    tabItem(
      tabName = "global",
      pre(prismCodeBlock(includeText("global.R"))),
    ),
    tabItem(
      tabName = "ui",
      pre(prismCodeBlock(includeText("ui.R")))
    ),
    tabItem(
      tabName = "server",
      pre(prismCodeBlock(includeText("server.R")))
    ),
    tabItem(
      tabName = "functions",
      div(prismCodeBlock(includeText("./R/functions.R")))
    ),
    tabItem(
      tabName = "modules",
      div(prismCodeBlock(includeText("R/modules.R")))
    ))
}
)

dashboardPagePlus(
  leafletjs,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "prism.css"),
    tags$script(src = "prism.js")
  ),
  header = header,
  sidebar = sidebar,
  body = body, enable_preloader = F
)

