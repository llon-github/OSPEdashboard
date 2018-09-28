# Load packages -----------------------------------------------------
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(DT)
library(readxl)
library(RSQLite)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

# UI ----------------------------------------------------------------
ui <- dashboardPage(
  
  # header code
  dashboardHeader(title = "OSPE Database",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  ),
                  dropdownMenu(type = "notifications", notificationItem(
                    text = "5 new users today",
                    icon("users")
                  ),icon = icon("github"))
  ),
  
  # sidebar code
  dashboardSidebar(
    
    # sidebar menus; maybe include Jon's nightly for easy access(?)
    sidebarMenu(id = "sidebarMenu",
                menuItem("Dashboard", tabName ="dashboard", icon = icon("dashboard")),
                menuItem("Catalog", tabName ="catalog", icon = icon("database")),
                menuItem("Bits and Bytes Articles", tabName = "bitsandbytes", icon = icon("file")),
                menuItem("Nightly/Weekly Regression", tabName = "nightly-weekly", icon = icon("calendar"),
                         menuSubItem("Cypress Nightly", href = "http://emcmark1/cypress-nightly-links.html"),
                         menuSubItem("Elm Nightly", href = "http://emcmark1/elm-nightly-links.html"),
                         menuSubItem("Elm-SR Nightly", href = "http://emcmark1/elm-sr-nightly-links.html"),
                         menuSubItem("Elm-SR Weekly", href = "http://emcmark1/elm-sr-weekly-links.html"),
                         menuSubItem("Foxtail Nightly", href = "http://emcmark1/early-foxtail-nightly-links.html")
                        ),
                menuItem("Machine List", tabName = "machinelist", icon = icon("list-ul"),
                         menuSubItem("OSPE Boxes", tabName = "ospeboxes"),
                         menuSubItem("DellEMC Boxes", tabName = "dellemcboxes"))
    )
  ),
  
  dashboardBody(

    fluidRow(
    # tabItems used to display different content and creates a new "mainPanel()"

      tabItems(
        #tabItem("dashboard"),
        tabItem("catalog", uiOutput("list"), uiOutput("char"), column(11, offset = 7, uiOutput("dbsim"))),
        tabItem("ospeboxes", uiOutput("boxlist")),
        tabItem("bitsandbytes", "Bits-N-Bytes Library", verbatimTextOutput(("print"))),
        tabItem("dellemcboxes", h2("DellEMC Boxes") , htmlOutput("boxeshtml"))
        )
      )
    ),
      # box(
      #   title = "Controls",
      #   sliderInput("slider", "Number of observations:", 1, 100, 50),
      #   
      #   # change plot properties
      #   selectInput("plot_type",
      #               label = "Plot Type",
      #               choices = c("Scatter" = "scatter",
      #                           "Bar" = "column",
      #                           "Line" = "line")),
      #   selectInput("theme",
      #               label = "Plot Theme",
      #               choices = c("No theme",
      #                           "Chalk" = "chalk",
      #                           "Dark Unica" = "darkunica",
      #                           "Economist" = "economist"
      #               )),
      #   
      #   # timeperiod choice
      #   dateRangeInput("dateFilter", label =paste('Date Range'),
      #                  start = Sys.Date() - 7, end = Sys.Date(), separator = " - ", format = "mm-dd-yyyy",
      #                  startview = 'month', language = 'en', weekstart = 0),
      #   verbatimTextOutput("dateValue")
      # ),
      # 
      # tags$head(tags$style(HTML('
      # .main-header .logo {
      #   font-family: "Arial", "Helvetica", sans-serif;
      #   font-weight: bold;
      #   font-size: 24px;
      # }
      # '))),

      #highchartOutput("plot", height = "300px"),
  # window color
  skin = "purple"
)


# SERVER ------------------------------------------------------------
server = function(input, output, session) {
  
  # change form of SQLquery based on the filters used
  filterCheck <- function(){
    size <- length(input$typeFilter)
    return(size)
  }
  
  # call to query DB; pass in value depending on filters applied for box config
  dbBoxQuery <- function(config){
    # box filters
    if (config=="250F") {
      sqlitePath <- "db//master.db"
      passedValue <- "VMax250F"
    } else if (config=="950F"){
      sqlitePath <- "db//master.db"
      passedValue <- "VMax950F"
    } else if (config=="PowerMax2000"){
      sqlitePath <- "db//master.db"
      passedValue <- "PMax2000"
    } else if (config=="PowerMax8000"){
      sqlitePath <- "db//master.db"
      passedValue <- "PMax8000"
    }
    
    query <- paste0("SELECT Model, Release, Ucode, Raid, Features, DbName FROM catalog WHERE Model LIKE '%", passedValue, "%'")
    db <- dbConnect(SQLite(), dbname=sqlitePath)
    dbdata <- dbGetQuery(db, query)
    dbDisconnect(db)

    return(dbdata)
  }
  
  # call to query DB; pass in value depending on filters applied for number of engines
  dbEngineQuery <- function(engines){
    # engine filters
    if (engines=="1") {
      sqlitePath <- "db//master.db"
      passedValue <- "1Engine"
    } else if (engines=="2"){
      sqlitePath <- "db//master.db"
      passedValue <- "2Engine"
    } else if (engines=="8"){
      sqlitePath <- "db//master.db"
      passedValue <- "8Engine"
    }
    
    query <- paste0("SELECT Model, Release, Ucode, Raid, Features, TestPath FROM catalog WHERE Model LIKE '%", passedValue, "%'")
    db <- dbConnect(SQLite(), dbname=sqlitePath)
    dbdata <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    return(dbdata)
  }
  
  # call to query DB; pass in value depending on filters applied for box features
  dbFeaturesQuery <- function(features){
    # features filters
    if (features=="Uncompressed") {
      sqlitePath <- "db//master.db"
      passedValue <- "None"
    } else if (features=="Compression"){
      sqlitePath <- "db//master.db"
      passedValue <- "COMP"
    } else if (features=="DeDupe"){
      sqlitePath <- "db//master.db"
      passedValue <- "DEDUP"
    } else if (features=="PowerPath"){
      sqlitePath <- "db//master.db"
      passedValue <- "POWER"
    } else if (features=="DARE"){
      sqlitePath <- "db//master.db"
      passedValue <- "DARE"
    }
    
    query <- paste0("SELECT Model, Release, Ucode, Raid, Features, TestPath FROM catalog WHERE Features LIKE '%", passedValue, "%'")
    db <- dbConnect(SQLite(), dbname=sqlitePath)
    dbdata <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    return(dbdata)
  }
  
  # get every row from DB
  dbFullQuery <- function(features){
    sqlitePath <- "db//master.db"
    query <- paste0("SELECT CatID, Model, Release, Ucode, Raid, Features, TestPath FROM catalog")
    db <- dbConnect(SQLite(), dbname=sqlitePath)
    dbdata <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    # convert char columns to factor columns to selectize via filter box
    dbdata$Model <- factor(dbdata$Model, ordered = TRUE)
    dbdata$Release <- factor(dbdata$Release, ordered = TRUE)
    dbdata$Ucode <- factor(dbdata$Ucode, ordered = TRUE)
    dbdata$Raid <- factor(dbdata$Release, ordered = TRUE)
    dbdata$Features <- factor(dbdata$Features, ordered = TRUE)
    
    return(dbdata)
  }
  
  
  # first display on the catalog tab
  # output$list <- renderUI({
  #   if (is.null(input$typeFilter)) {
  #     tabBox(title = "OSPE Catalog", id = "tabset1", height = "250px", width = 9,
  #            tabPanel("Database", value = "datapanel", output$query <- DT::renderDT({
  #              sqlitePath <- "db//master.db"
  #              query <- paste0("SELECT Model, Release, Ucode, Raid, Features, DbName FROM catalog")
  #              db <- dbConnect(SQLite(), dbname=sqlitePath)
  #              data <- dbGetQuery(db, query)
  #              dbDisconnect(db)
  # 
  #              return(data)}, options = list(pageLength = 15, lengthChange = FALSE))),
  #            tabPanel("Graphs", value = "graphpanel", "NULL tab"),
  #            tabPanel("SX_Data", value = "metadatapanel", "NULL tab"))
  #   }
  # })
  
  # sidebarMenu triggers
  observeEvent(input$sidebarMenu, {
    if (input$sidebarMenu=="dashboard") {
      #output$catalogprint <- renderPrint({input$typeFilter})
      
    }
    
    if (input$sidebarMenu=="catalog") {
      # output$filters <- renderUI({ 
      #   box(title = "Filters", status = "primary", width = 3,
      #     dateRangeInput("dateFilter", label =paste('Date Range'),
      #                     start = Sys.Date() - 7, end = Sys.Date(), separator = " - ", format = "mm-dd-yyyy",
      #                     startview = 'month', language = 'en', weekstart = 0),
      #     checkboxGroupInput("typeFilter", label = h3("Box Type"),
      #                        choices = list("250F" = "250F", "950F" = "950F", "PowerMax2000" = "PowerMax2000",
      #                                       "PowerMax8000" = "PowerMax8000" )),
      #     checkboxGroupInput("engineFilter", label = h3("Engines"),
      #                        choices = list("1" = "1", "2" = "2", "8" = "8"),
      #                        selected = NULL),
      #     checkboxGroupInput("featuresFilter", label = h3("Features"),
      #                        choices = list("Uncompressed" = "Uncompressed", "Compression" = "Compression",
      #                                       "DeDupe" = "DeDupe", "PowerPath" = "PowerPath", "D@RE" = "DARE" ),
      #                        selected = NULL),
      #     br(),
      #     actionButton("graph", "Add to graphs..."),
      #     actionButton("export", "Export to DB2XL...")
      #   )
      # })
      
      output$list <- renderUI({
        tabBox(title = "OSPE Catalog", id = "tabset1", height = "250px", width = 7,
               tabPanel("Database", DT::dataTableOutput("dbTable")),
               tabPanel("Graphs", verbatimTextOutput("debug")),
               tabPanel("Metadata", paste0("y")))
      })
      
      output$char <- renderUI({
        box(title = "Characterization", status = "primary", width = 5, highchartOutput("charHC", height = "290px"))
      })
      
      output$dbsim <- renderUI({
        box(title = "DBsim", status = "primary", width = 5, highchartOutput("dbHC", height = "290px"))
      })
      
      output$charHC <- renderHighchart({
         highchart() %>%
          hc_add_series(data = c(4444,20000,44789,73785,100000), type = "column") %>%
          hc_plotOptions(column = list(colorByPoint = TRUE)) %>%
          hc_yAxis(title = list(text = "IOps")) %>%
          hc_xAxis(title = list(text = "Workloads")) %>%
          hc_title(
            text = "Test title") 
        
          #hc_add_series(data = purrr::map(0:4, function(x) list(x, x)), type = "scatter", color = "red")
      })
      
      output$dbHC <- renderHighchart({
        highchart() %>%
          #hc_add_series(data = abs(rnorm(5)), type = "column") %>%
          hc_add_series(data = c(0.42, 0.53, 0.69, 1.81, 4.12), type = "line", color = "blue") %>%
        hc_title(
          text = "Test title") 
      })
      
      
      dbdata = dbFullQuery()
      output$dbTable <- DT::renderDT({dbdata}, options = list(pageLength = 14, lengthChange = FALSE, autoWidth = TRUE, dom = 'tp', scrollX = TRUE, 
                                                              columnDefs = list(list(width = '150px', targets = c(1,2,3,4,5,6)))), 
                                     filter = list(position = 'top', clear = FALSE), rownames = FALSE)
    }
    
    if (input$sidebarMenu=="ospeboxes") {
      output$boxlist <- renderUI({
        box(title = "OSPE Boxes", status = "primary", width = 12,
            output$renderOSPEboxes <- DT::renderDT({
              location = "\\\\efsus03\\pdata\\OSPE_Group_Folder\\dashboard\\machinelist.xlsx"
              read_xlsx(paste(location))
            }, options = list(pageLength = 5, lengthChange = FALSE))
        )
      })
    }
    
    if (input$sidebarMenu=="dellemcboxes") {
      getPage<-function(){
        return(includeHTML("\\\\efsus03\\pdata\\OSPE_Group_Folder\\dashboard\\boxlist.htm"))
      }
      output$boxeshtml<-renderUI({getPage()})
    }
    
    if (input$sidebarMenu=="bitsandbytes") {
      output$print <- renderPrint({"testo"})
    }
  })
  
  # boxFilter triggers
  observeEvent(input$typeFilter, {
    # change to database panel when button clicked
    updateTabsetPanel(session, "tabset1", selected = paste0("Database"))
    
    # variable storage for multiple checkbox inputs
    vmax250F <- "250F"  %in% input$typeFilter
    vmax950F <- "950F"  %in% input$typeFilter
    pmax2000 <- "PowerMax2000"  %in% input$typeFilter
    pmax8000 <- "PowerMax8000"  %in% input$typeFilter

    if (vmax250F) {
        filter <- "250F"
        dbdata = dbBoxQuery(filter)
    }

    if (vmax950F) {
        filter <- "950F"
        dbdata = dbBoxQuery(filter)
    }

    if (pmax2000) {
        filter <- "PowerMax2000"
        dbdata = dbBoxQuery(filter)
    }

    if (pmax8000) {
        filter <- "PowerMax8000"
        dbdata = dbBoxQuery(filter)
    }
    
    # use dbTable reference to dynamically change just the chart instead of re-rendering
    output$dbTable <- DT::renderDT({dbdata}, options = list(pageLength = 15, lengthChange = FALSE, scrollX = TRUE))
    
    
  })
  
  # engine triggers
  observeEvent(input$engineFilter, {
    # change to database panel when button clicked
    updateTabsetPanel(session, "tabset1", selected = paste0("Database"))
    
    # variable storage for multiple checkbox inputs
    engines1 <- "1"  %in% input$engineFilter
    engines2 <- "2"  %in% input$engineFilter
    engines8 <- "8"  %in% input$engineFilter
    
    if (engines1) {
        filter <- "1"
        dbdata = dbEngineQuery(filter)
    }
    
    if (engines2) {
        filter <- "2"
        dbdata = dbEngineQuery(filter)
    }
    
    if (engines8) {
        filter <- "8"
        dbdata = dbEngineQuery(filter)
    }
    
    # use dbTable reference to dynamically change just the chart instead of re-rendering
    output$dbTable <- DT::renderDT({dbdata}, options = list(pageLength = 15, lengthChange = FALSE))
  })
  
  # features triggers
  observeEvent(input$featuresFilter, {
    # change to database panel when button clicked
    updateTabsetPanel(session, "tabset1", selected = paste0("Database"))
    
    # variable storage for multiple checkbox inputs
    noComp <- "Uncompressed"  %in% input$featuresFilter
    comp <- "Compression"  %in% input$featuresFilter
    dedupe <- "DeDupe"  %in% input$featuresFilter
    powerpath <- "PowerPath"  %in% input$featuresFilter
    dare <- "DARE"  %in% input$featuresFilter
    
    if (noComp) {
        filter <- "Uncompressed"
        dbdata = dbFeaturesQuery(filter)
    }
    
    if (comp) {
        filter <- "Compression"
        dbdata = dbFeaturesQuery(filter)
    }
    
    if (dedupe) {
        filter <- "DeDupe"
        dbdata = dbFeaturesQuery(filter)
    }
    
    if (powerpath) {
        filter <- "PowerPath"
        dbdata = dbFeaturesQuery(filter)
    }
    
    if (dare) {
        filter <- "DARE"
        dbdata = dbFeaturesQuery(filter)
    }
    
    # use dbTable reference to dynamically change just the chart instead of re-rendering
    output$dbTable <- DT::renderDT({dbdata}, options = list(pageLength = 15, lengthChange = FALSE))
    
  })
  
  storedSelections <- reactiveValues()
  
  observeEvent(input$graph,{
    # change to graph panel when button clicked
    updateTabsetPanel(session, "tabset1", selected = paste0("Graphs"))
    
    # reactive expression to store data from previous selections over to a new graph
    storedSelections$dList <- c(isolate(storedSelections$dList), isolate(input$dbTable_rows_selected))
  
    output$debug <- renderPrint({
      storedSelections$dList
    })
  })
  
  observeEvent(input$type,{
    
  })
  
  observeEvent(input$export,{
    
    # output$list <- renderUI({
    #   box("hi", input$dbTable_rows_selected)
    # })
    system('ruby "C:/Users/lonl/Documents/Ruby/Post-Processing-v2/Main.rb"')
  })
  
  output$plot <- renderHighchart({
    
  })
}

# Run app -----------------------------------------------------------
shinyApp(ui = ui, server = server)
