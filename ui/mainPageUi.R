"  
  Author: Hèctor Garcia Guillen
  Name: loadFiles.R 
  Description:
  Date: 17/06/07
  Version: 1.1
"

tabItem(tabName = "mainPage",
        fluidRow(
          box(
            width = 12,
            title = div(
              id = "firstInput",
              # Contains: - Input gene.stable.ID
              #           - List of related events with checkbox
              #           - Action button to init search
              box(
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",
                selected = "Gene.stable.ID",
                rezisable = T,
                width = 11.5,
                title = "Browser",
                div(
                  bsButton(
                    inputId = "add", 
                    label = "Add Track"#, 
                    #style = "primary"#,
                    #icon = icon("chevron-right")
                  ),
                  bsButton(
                    inputId = "remove", 
                    label = "Remove Track"#, 
                    #style = "primary"#,
                    #icon = icon("chevron-right")
                  ),
                  bsButton(
                    inputId = "change", 
                    label = "Change Filters"#, 
                    #style = "primary"#,
                    #icon = icon("chevron-right")
                  ),
                  bsButton(
                    inputId = "upload", 
                    label = "Upload Files"#,
                    #style = "primary"#,
                    #icon = icon("chevron-right")
                  )
                ),
                div(
                  bsButton(
                    inputId = "list", 
                    label = NULL, 
                    icon = icon("glyphicon glyphicon-list", lib = "glyphicon")#,
                    #style = "info"#,
                  ),
                  textInput(
                    inputId = "inputSearch",
                    value = "", #ENSG00000000003 #MET #chr1:169818772-169863408:-
                    label = NULL,
                    #disabled = T,
                    placeholder = "Gene.stable.ID / Gene.name / Location"
                  ),
                  
                  
                  # Action button to init search
                  shinyjs::useShinyjs(),
                  bsButton(
                    inputId = "goButton", 
                    label = "Go!",
                    #style = "info",
                    disabled = T
                  )
                ),
                # List of related events with checkbox
                conditionalPanel("!is.null(v.data)",
                                 uiOutput("eventList")
                ),
                div(
                  bsButton(
                    inputId = "clear", 
                    label = NULL, 
                    icon = icon("glyphicon glyphicon-trash", lib = "glyphicon")#,
                    #style = "info"#,
                  ),
                  textInput(
                    inputId = "location_str", 
                    label = NULL, 
                    placeholder = "Genomic location"
                  ),
                  verbatimTextOutput("loc"),
                  bsButton(
                    inputId = "go_left", 
                    label = NULL, 
                    disabled = T,
                    icon = icon("chevron-left")#,
                    #style = "primary"#,
                  ),
                  bsButton(
                    inputId = "zoom_in", 
                    label = NULL, 
                    disabled = T,
                    icon = icon("glyphicon glyphicon-zoom-in", lib = "glyphicon")#,
                    #style = "primary"#,
                  ),
                  bsButton(
                    inputId = "zoom_out", 
                    label = NULL, 
                    disabled = T,
                    icon = icon("glyphicon glyphicon-zoom-out", lib = "glyphicon")#,
                    # = "primary"#,
                  ),
                  bsButton(
                    inputId = "go_right", 
                    label = NULL,
                    disabled = T,
                    icon = icon("chevron-right")#,
                    #style = "primary"#,
                  ) 
                )
              ),
              uiOutput("geneList"),
              uiOutput("uploadFiles"),
              uiOutput("removeTracks"),
              uiOutput("addTracks")
            ),
            collapsible = T,
            solidHeader = T,
            status = 'primary',
            plotOutput("genome_plot", height = 600)
          ),
          
          # Contains: - Dinamic multi plot result
          box(
            title = div(
              # Contains the filters to search
              box(
                id = "tabset2",
                selected = "Gene.stable.ID",
                width = 11.5,
                title = "Boxplot",
                tabBox(
                  selected = "Clinical information",
                  width = 11.5,
                  # Panel for filter Clinical information
                  tabPanel(
                    "Clinical information",
                    radioButtons(
                      inputId = "dataType", 
                      label = "",
                      choices = c("OUTCOME" = "OUTCOME",
                                  "MICROSTAGING_AB" = "MICROSTAGING_AB",
                                  "MICROPAPILARYP_ATTERN" = "MICROPAPILARYP_ATTERN"),
                      selected = c("OUTCOME")               
                    )
                  ),
                  # Panel for filter Patient Type
                  tabPanel(
                    "Patient Type", 
                    radioButtons(
                      inputId = "patientType", 
                      label = "",
                      choices =c(
                        "All" = "All",
                        "vh" = "vh",
                        "mp" = "mp"),
                      selected = c("All") 
                    )
                  )
                )
              )
            ),
            plotlyOutput("multiPlot", width = "100%"), 
            align = "center",
            width = 12,
            collapsible = T,
            solidHeader = T,
            status = 'primary',
            textOutput("gene"),
            textOutput("dataType"),
            textOutput("patientType"),
            textOutput("outPutText3"),
            textOutput("outPutText4"),
            textOutput("outPutText5")
          ),
          # Contains: - multi plot generated from related data
          box(
            collapsible = T,
            collapsed = T,
            solidHeader = T,
            title = "Check data",
            align = "center",
            width = 12,
            status = 'primary',
            dataTableOutput("dataResults")
          )
        )
)
