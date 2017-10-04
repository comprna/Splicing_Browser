
"  
  Author: Hèctor García Guillén
  Name: app.R 
  Description: 
  Date: 17/06/07
  Version: 1.1
"

######################################################################################

# Install and load all necessary libreries to run app
#source(file.path("server", "scripts", "loadLibreries.R"))

# Load all files to execute multiplot and gene browser funtionalities
#source(file.path("server", "scripts", "loadFiles.R"), local = TRUE)$value

# If RStudio shows error "" execute commands:
# 1 - unloadNamespace("shiny")
# 2 - require(shiny)

######################################################################################

# change max upload files
options(shiny.maxRequestSize=300*1024^2)
# delete warning messages
options(warn=-1)

"
  Name: ui
  Description: contain all interface code
  Date: 17/06/07
  Version: 1.1
  Param: skin, Change the dashboard interface color
         header, Page header
         sidebar, Page sidebar, menu
         body, Page body
  Return: none
"
ui <- dashboardPage(
  
  # Change the dashboard interface color
  skin = "blue",
  
  # Page header
  header <- dashboardHeader(
    title = "Alternative Splicing Browser",
    titleWidth = "100%"
  ), 
  
  # Page sidebar, menu
  sidebar <- dashboardSidebar(
    width = 200,
    disable = T,
    sidebarMenu(
      id = "mainSidebar",
      collapsed = T,
      menuItem(
        text = "Gene Browser B", 
        tabName = "mainPage", 
        icon = icon("glyphicon glyphicon-search", lib = "glyphicon")
        )
    )
  ), 
  
  # Page body
  body <- dashboardBody(
    # head tag to linck myself stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # Body content
    tabItems(
      source(file.path("ui", "mainPageUi.R"),  local = TRUE)$value
    )#, source(file.path("ui","footer.R"),  local = TRUE)$value
  )
)

######################################################################################

"
  Name: server
  Description: contain all server code
  Date: 17/06/07
  Version: 1.1
  Param: input, 
         output, 
         session, 
  Return: none
"
server <- function(input, output, session) {
  source(file.path("server", "mainPageServer.R"),  local = TRUE)$value
}

######################################################################################

if (interactive()) {
  shinyApp(ui, server)
}
