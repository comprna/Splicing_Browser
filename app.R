
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


######################################################################################



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
if (interactive()) {
  shinyApp(
    ui <- dashboardPage(
      # Change the dashboard interface color
      #skin = "blue",
      # Page header
      dashboardHeader(
        title = "SUPPA Browser",
        titleWidth = "100%"
      ), 
      # Page sidebar, menu
      dashboardSidebar(
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
      dashboardBody(
        useShinyjs(),
        # includeCSS to link myself stylesheet
        includeCSS("www/custom.css"),
        # Body content
        # Loading message
        tags$div(
          id = "loading-content",
          h2("Loading...")
        ),
        hidden(
          tags$div(
            id = "app-content",
            p("Testing loading gif."))
        ),
        # The main app code goes here
        source(file.path("ui", "mainPageUi.R"),  local = TRUE)$value
        , source(file.path("ui","footer.R"),  local = TRUE)$value
      )
    ),
    
    server <- function(input, output, session) {
      # change max upload files
      options(shiny.maxRequestSize=3000*1024^2)
      # delete warning messages
      options(warn=-1)
      source(file.path("server", "scripts", "loadFiles.R"), local = TRUE)$value
      source(file.path("server", "mainPageServer.R"),  local = TRUE)$value
      hide(id = "loading-content", anim = TRUE, animType = "fade")    
      show("app-content")
    }
  )
}
