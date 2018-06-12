library(shiny)
library(DT)
library(dplyr)
library(leaflet)
library(htmltools)



ui <-   navbarPage(title = "Boreal Lynx Project Collar Viewer",
                   id = "nav",
                   tabPanel('Input Data', div(class = "pg1",
                                              fluidPage(
                                                fluidRow(
                                                  column(width=3,
                                                         #hr("Load Data"),
                                                         hr(),
                                                         img(src="AIM.png", align = "center"),
                                                         fileInput("file", "UPLOAD FILE"),
                                                         hr(),
                                                         # selectInput(inputId = "si.species",
                                                         #             label = "Species:",
                                                         #             choices = c("Mountain goat" = "Mountain goat",
                                                         #                         "Lynx" = "Lynx"),
                                                         #             selected = c("Lynx"),
                                                         #             multiple = TRUE),
                                                         # selectInput(inputId = "si.mgmtarea",
                                                         #             label = "Management Area:",
                                                         #             choices = c("Kodiak Refuge" = "Kodiak Refuge",
                                                         #                         "Tetlin Refuge" = "Tetlin Refuge",
                                                         #                         "KNI Refuge" = "KNI Refuge",
                                                         #                         "Yukon Flats Refuge" = "Yukon Flats Refuge",
                                                         #                         "Kanuti Refuge" = "Kanuti Refuge",
                                                         #                         "Other" = "Other"),
                                                         #             selected = c("Tetlin Refuge",
                                                         #                          "KNI Refuge",
                                                         #                          "Yukon Flats Refuge",
                                                         #                          "Kanuti Refuge",
                                                         #                          "Other"),
                                                         #             multiple = TRUE),
                                                         # selectInput(inputId = "si.collarID",
                                                         #             label = "Collar ID:",
                                                         #             choices = levels(dat$id),
                                                         #             selected = levels(dat$id),
                                                         #             multiple = TRUE,
                                                         #             selectize = TRUE),
                                                         dateRangeInput(inputId = "dr.dates",
                                                                        label = "Date Range:",
                                                                        start = "2018-01-01",
                                                                        format = "mm/dd/yyyy",
                                                                        sep = "/"),
                                                         sliderInput(inputId = "si.month",
                                                                     label = "Select a month:",
                                                                     min = 1,
                                                                     max = 12,
                                                                     value = c(1, 12)),
                                                         actionButton(inputId = "goButton",
                                                                      label = "Go!"),
                                                         downloadButton(outputId = "downloadData", 
                                                                        label = "Download")
                                                  ),
                                                  
                                                  column(width=9,
                                                         tabsetPanel(type = "tabs",
                                                                     tabPanel("Map",
                                                                              leafletOutput("mymap", height = "800px")),
                                                                     tabPanel("Summary Table", DT::dataTableOutput(outputId = "dt.animals"))))
                                                )
                                              ))
                   ),
                   
                   # HOME RANGE ANALYSES
                   tabPanel("Home Ranges",
                            fluidPage(
                              fluidRow(
                                column(width=3,
                                       hr())
                              ))
                   ),
                   
                   # REPORT
                   tabPanel("Reports",
                            fluidPage(
                              fluidRow(
                                column(width=3,
                                       h4("Make your selection for your report"),
                                       hr(),
                                       # selectInput(inputId = "si.mgmtarea",
                                       #             label = "Management Area:",
                                       #             choices = c("Kodiak Refuge" = "Kodiak Refuge",
                                       #                         "Tetlin Refuge" = "Tetlin Refuge",
                                       #                         "KNI Refuge" = "KNI Refuge",
                                       #                         "Yukon Flats Refuge" = "Yukon Flats Refuge",
                                       #                         "Kanuti Refuge" = "Kanuti Refuge",
                                       #                         "Other" = "Other"),
                                       #             selected = c(""),
                                       #             multiple = TRUE),  
                                       dateRangeInput(inputId = "dr.dates",
                                                      label = "Date Range:",
                                                      start = "2014-08-01",
                                                      format = "mm/dd/yyyy",
                                                      sep = "/"),
                                       hr(),
                                       downloadButton("report", "Generate a report")
                                )
                              ))
                   ),
                   
                   # ABOUT PAGE
                   tabPanel("About",
                            h3("Exploratory Analysis and Visualization of Lynx Space Use"),
                            hr(),
                            h4("Version: 0.1"),
                            h5("Contact: McCrea Cobb"),
                            h5("Email: mccrea_cobb@fws.gov"),
                            img(src="AIM.png", align = "center")
                   )
                   
)







server <- function(input, output) {
  
  dat <- reactive({                             # import the data (.csv)
    file1 = input$file
    if(is.null(file1)){return()}
    x  <- read.csv(file = file1$datapath,
                   header = TRUE,
                   stringsAsFactors = TRUE)
    x$fixtime <- as.POSIXct(strptime(x$fixtime, format = "%Y-%m-%d %H:%M"))
    x$date <- as.Date(x$fixtime)
    x$month <- as.numeric(format(x$date, "%m"))
    x$lon <- as.numeric(x$lon)
    x$lat <- as.numeric(x$lat)
    # x$species <- "Lynx"
    # x$mgmtarea <- "Tetlin"
    x <- x[complete.cases(x$lat), ]   # Remove NA rows from lat/lon
    x
  })
  
  dat.sub <- eventReactive(input$goButton, {          # Subsets the data based on user inputs
    dat() %>%
      dplyr::filter(# species == input$si.species,
                    # mgmtarea == input$si.mgmtarea,
                    # ndowid == input$si.collarID,
                    date > input$dr.dates[1] & date < input$dr.dates[2],
                    month >= input$si.month[1] & month <= input$si.month[2])
  })
  
  dat.sub.tbl <- reactive({
    dat.sub() %>% group_by("CollarID" = id) %>%  # Creates a summary table of the data
      summarise(
        # "Location" = first(mgmtarea),
        # "Species" = first(species),
        "FirstFix" = min(date),
        "LastFix" = max(date),
        "Lat" = last(lat),
        "Long" = last(lon)
      )
  })
  
  output$dt.animals <- DT::renderDataTable({        # Displays a summary table of the data
    # print(head(dat.sub()))
    # print(table(dat.sub()$month))
    DT::datatable(dat.sub.tbl(), options = list(pageLength = 10))
  })
  
  # Create a map of the subsetted data
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addProviderTiles(provider = "OpenTopoMap",
                       group = "Basemap") %>%
      addProviderTiles(provider = "Esri.WorldImagery",
                       group = "Sat imagery") %>%
      # Overlay groups:
      addCircleMarkers(data = dat.sub(),
                       lng = dat.sub()$lon,
                       lat = dat.sub()$lat,
                       radius = 0.5,
                       label = paste0("ID:", dat.sub.tbl()$CollarID),
                       group = "All fixes") %>%
      addCircleMarkers(data = dat.sub.tbl(),
                       lng = dat.sub.tbl()$Long,
                       lat = dat.sub.tbl()$Lat,
                       radius = 1.0,
                       color = "orange",
                       label = paste0("Last fix: ", dat.sub.tbl()$LastFix),
                       group = "Last fix") %>%
      # addPolylines(data = dat.sub(),
      #              lng = dat.sub()$lng,
      #              lat = dat.sub()$lat,
      #              group = dat.sub()$ndowid) %>%
      # Layer controls:
      addLayersControl(
        baseGroups = c("Basemap", "Sat imagery"),
        overlayGroups = c("All fixes", "Last fix"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      addMiniMap(position = "bottomright") %>%
      addMeasure()
  )
  
  # Download the table as a .csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("CollarData_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat.sub(), file, row.names = FALSE)
    }
  )
  
  # Generate a markdown report (A work in progress.....)
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copies the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}



shinyApp(ui, server)
