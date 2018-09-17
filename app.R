# Load packages -----------------------------------------------------
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
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
        #tabItem("dashboard"), highchartOutput("plot"),
        tabItem("catalog", uiOutput(("filters")), uiOutput(("list")), DT::dataTableOutput("dbTable")),
        tabItem("ospeboxes", uiOutput("boxlist")),
        tabItem("bitsandbytes", "Bits-N-Bytes Library", verbatimTextOutput(("print"))),
        tabItem("dellemcboxes", h2("DellEMC Boxes") , htmlOutput("boxeshtml"))
        )
      )
    ),
    
    verbatimTextOutput("debug"),
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
    data <- dbGetQuery(db, query)
    dbDisconnect(db)

    return(data)
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
    
    query <- paste0("SELECT Model, Release, Ucode, Raid, Features, DbName FROM catalog WHERE Model LIKE '%", passedValue, "%'")
    db <- dbConnect(SQLite(), dbname=sqlitePath)
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    return(data)
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
    
    query <- paste0("SELECT Model, Release, Ucode, Raid, Features, DbName FROM catalog WHERE Features LIKE '%", passedValue, "%'")
    db <- dbConnect(SQLite(), dbname=sqlitePath)
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    return(data)
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
      output$filters <- renderUI({ 
        box(title = "Filters", status = "primary", width = 3,
          dateRangeInput("dateFilter", label =paste('Date Range'),
                          start = Sys.Date() - 7, end = Sys.Date(), separator = " - ", format = "mm-dd-yyyy",
                          startview = 'month', language = 'en', weekstart = 0),
          checkboxGroupInput("typeFilter", label = h3("Box Type"),
                             choices = list("250F" = "250F", "950F" = "950F", "PowerMax2000" = "PowerMax2000",
                                            "PowerMax8000" = "PowerMax8000" ), selected = "250F"),
          checkboxGroupInput("engineFilter", label = h3("Engines"),
                             choices = list("1" = "1", "2" = "2", "8" = "8"),
                             selected = NULL),
          checkboxGroupInput("featuresFilter", label = h3("Features"),
                             choices = list("Uncompressed" = "Uncompressed", "Compression" = "Compression",
                                            "DeDupe" = "DeDupe", "PowerPath" = "PowerPath", "D@RE" = "DARE" ),
                             selected = NULL),
          br(),
          actionButton("graph", "Import data..."),
          actionButton("export", "Export to DB2XL...")
        )
      })
      
      
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
    # variable storage for multiple checkbox inputs
    vmax250F <- "250F"  %in% input$typeFilter
    vmax950F <- "950F"  %in% input$typeFilter
    pmax2000 <- "PowerMax2000"  %in% input$typeFilter
    pmax8000 <- "PowerMax8000"  %in% input$typeFilter

    if (vmax250F) {
        filter <- "250F"
        data = dbBoxQuery(filter)
    }

    if (vmax950F) {
        filter <- "950F"
        data = dbBoxQuery(filter)
    }

    if (pmax2000) {
        filter <- "PowerMax2000"
        data = dbBoxQuery(filter)
    }

    if (pmax8000) {
        filter <- "PowerMax8000"
        data = dbBoxQuery(filter)
    }
    
    output$list <- renderUI({
      tabBox(title = "OSPE Catalog", id = "tabset1", height = "250px", width = 9,
             tabPanel("Database", output$dbprint <- DT::renderDT({data}, options = list(pageLength = 15, lengthChange = FALSE))),
             tabPanel("Graphs", paste0(filter)),
             tabPanel("Metadata", paste0(filter)))
    })
    
  })
  
  # engine triggers
  observeEvent(input$engineFilter, {
    # variable storage for multiple checkbox inputs
    engines1 <- "1"  %in% input$engineFilter
    engines2 <- "2"  %in% input$engineFilter
    engines8 <- "8"  %in% input$engineFilter
    
    if (engines1) {
        filter <- "1"
        data = dbEngineQuery(filter)
    }
    
    if (engines2) {
        filter <- "2"
        data = dbEngineQuery(filter)
    }
    
    if (engines8) {
        filter <- "8"
        data = dbEngineQuery(filter)
    }
    
    output$list <- renderUI({
      tabBox(title = "OSPE Catalog", id = "tabset1", height = "250px", width = 9,
             tabPanel("Database", output$dbprint <- DT::renderDT({data}, options = list(pageLength = 15, lengthChange = FALSE))),
             tabPanel("Graphs", paste0(filter)),
             tabPanel("Metadata", paste0(filter)))
    })
    
  })
  
  # features triggers
  observeEvent(input$featuresFilter, {
    # variable storage for multiple checkbox inputs
    noComp <- "Uncompressed"  %in% input$featuresFilter
    comp <- "Compression"  %in% input$featuresFilter
    dedupe <- "DeDupe"  %in% input$featuresFilter
    powerpath <- "PowerPath"  %in% input$featuresFilter
    dare <- "DARE"  %in% input$featuresFilter
    
    if (noComp) {
        filter <- "Uncompressed"
        data = dbFeaturesQuery(filter)
    }
    
    if (comp) {
        filter <- "Compression"
        data = dbFeaturesQuery(filter)
    }
    
    if (dedupe) {
        filter <- "DeDupe"
        data = dbFeaturesQuery(filter)
    }
    
    if (powerpath) {
        filter <- "PowerPath"
        data = dbFeaturesQuery(filter)
    }
    
    if (dare) {
        filter <- "DARE"
        data = dbFeaturesQuery(filter)
    }
    
    output$list <- renderUI({
      tabBox(title = "OSPE Catalog", id = "tabset1", height = "250px", width = 9,
             tabPanel("Database", output$dbprint <- DT::renderDT({data}, options = list(pageLength = 15, lengthChange = FALSE))),
             tabPanel("Graphs", paste0(filter)),
             tabPanel("Metadata", paste0(filter)))
    })
  })
  
  observeEvent(input$export,{
    
    # output$list <- renderUI({
    #   box("hi", input$dbTable_rows_selected)
    # })
    #system('ruby "C:/Users/lonl/Documents/Ruby/Post-Processing-v2/Main.rb"')
  })
    
  output$plot <- renderHighchart({
    
  })
}

# Run app -----------------------------------------------------------
shinyApp(ui = ui, server = server)
