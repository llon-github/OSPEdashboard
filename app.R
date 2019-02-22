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
library(reshape2)

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
        tabItem("catalog", uiOutput("list")),
        tabItem("ospeboxes", uiOutput("boxlist")),
        tabItem("bitsandbytes", "Bits-N-Bytes Library", verbatimTextOutput(("print"))),
        tabItem("dellemcboxes", htmlOutput("boxeshtml"))
        )
      )
    ),
      
  skin = "purple"
)


# SERVER ------------------------------------------------------------
server = function(input, output, session) {
  
  # global data storage for future manipulation
  globalDbdata <- NULL
  paths <- NULL
  charList <- NULL
  dbList <- NULL
  
  # get every row from DB
  dbFullQuery <- function(){
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
  
  charTestQuery <- function(){
    # appending /Notes folder to testpaths from rows_selected
    testPaths <- paste0(paths,"/Notes/")
    writeType <- ""
    
    if (testPaths == "/Notes/"){
      # input$charWorkload is triggered on startup; skip searching for path when "catalog" is clicked
      print("Catalog Clicked (CHAR loop)")
    } else {
      testPaths <- paste0("//efsus03",testPaths)
      numPaths <- NROW(testPaths)
      
      for (j in 1:numPaths) {
        
        # take input box names and reference the data for that name in the DB
        converted <- input$charWorkload
        if (converted == "Random Read Hit"){
          converted = "random_read_100hit"
        }else if (converted == "Random Write Hit"){
          converted = "random_write_hit"
        }else if (converted == "Random Max Write"){
          writeType = "max"
          converted = "random_0read"
        }else if (converted == "Random Sustained Write"){
          writeType = "sustained"
          converted = "random_0read"
        }else if (converted == "Random Read Miss"){
          converted = "random_100read"
        }else if (converted == "Sequential Write"){
          converted = "sequential_write"
        }else if (converted == "Sequential Read"){
          converted = "sequential_read"
        }else{
          converted = "Unknown Workload"
        }
        
        sqliteFile <- list.files(testPaths[j], pattern="\\.db$")
        sqlitePath <- paste0(testPaths[j], sqliteFile)
        nameQuery <- paste0("SELECT DISTINCT Testname FROM char")
        configQuery <- paste0("SELECT DISTINCT ConfigStr FROM char")
        db <- dbConnect(SQLite(), dbname=sqlitePath)
        nameData <- dbGetQuery(db, nameQuery)
        configData <- dbGetQuery(db, configQuery)
        
        # number of unique workloads in CHAR table
        uniqCharTests = NROW(nameData)
        
        for(i in 1:uniqCharTests) {
          # special queries if sustained or max writes; else grab the IOPS column; want to grab full box but no definition; grabbing [1,1] 1st row, 1st column data
          if (writeType == "max") {
            charQuery <- paste0("SELECT Iteration, Testname, IOsize, Maxwrites FROM char WHERE Testname='",converted,"' AND ConfigStr='",configData[1,1],"' AND iteration=1")
            blkSizeQuery <- paste0("SELECT IOsize FROM char WHERE Testname='",converted,"' AND ConfigStr='",configData[1,1],"' AND iteration=1")
            sysQuery <- paste0("SELECT Model, Ucode FROM system")
          } else if (writeType == "sustained") {
            charQuery <- paste0("SELECT Iteration, Testname, IOsize, Suswrites FROM char WHERE Testname='",converted,"' AND ConfigStr='",configData[1,1],"' AND iteration=1")
            blkSizeQuery <- paste0("SELECT IOsize FROM char WHERE Testname='",converted,"' AND ConfigStr='",configData[1,1],"' AND iteration=1")
            sysQuery <- paste0("SELECT Model, Ucode FROM system")
          } else {
            charQuery <- paste0("SELECT Iteration, Testname, IOsize, IOPS FROM char WHERE Testname='",converted,"' AND ConfigStr='",configData[1,1],"' AND iteration=1")
            blkSizeQuery <- paste0("SELECT IOsize FROM char WHERE Testname='",converted,"' AND ConfigStr='",configData[1,1],"' AND iteration=1")
            sysQuery <- paste0("SELECT Model, Ucode FROM system")
          }
          charData <- dbGetQuery(db, charQuery)
          blkSizeData <- dbGetQuery(db, blkSizeQuery)
          systemData <- dbGetQuery(db, sysQuery)
        }
        # put everything into a list so we can pass all the data to an array @ index j
        charList[[j]] <- list(nameData, configData, charData, blkSizeData, systemData)
        print(charList)
      }
      dbDisconnect(db)
      return(charList)
    }
  }
  
  dbTestQuery <- function(){
    # appending /Notes folder to testpaths from rows_selected
    testPaths <- paste0(paths,"/Notes/")
    
    if (testPaths == "/Notes/"){
      # input$dbWorkload is triggered on startup; skip searching for path when "catalog" is clicked
      print("Catalog Clicked (DBsim loop)")
    } else{
      testPaths <- paste0("//efsus03",testPaths)
      numPaths <- NROW(testPaths)
      
      for (j in 1:numPaths) {
        # take input box names and reference the data for that name in the DB
        x <- input$dbWorkload
        
        sqliteFile <- list.files(testPaths[j], pattern="\\.db$")
        sqlitePath <- paste0(testPaths[j], sqliteFile)
        nameQuery <- paste0("SELECT DISTINCT Testname FROM dbsim")
        intervalQuery <- paste0("SELECT Intervals FROM dbsim WHERE Testname='",x,"' AND iteration=1")
        db <- dbConnect(SQLite(), dbname=sqlitePath)
        nameData <- dbGetQuery(db, nameQuery)
        intervalData <- dbGetQuery(db, intervalQuery)
      
        # number of unique workloads in CHAR table
        uniqDBTests = NROW(nameData)
      
        for(i in 1:uniqDBTests) {
          # if IM style workload
          if (intervalData == 7) {
            dbIOPSquery <- paste0("SELECT IOPS1,IOPS2,IOPS3,IOPS4,IOPS5,IOPS6,IOPS7 FROM dbsim WHERE Testname='",x,"' AND iteration=1")
            dbRTquery <- paste0("SELECT RT1,RT2,RT3,RT4,RT5,RT6,RT7 FROM dbsim WHERE Testname='",x,"' AND iteration=1")
            sysQuery <- paste0("SELECT Model, Ucode FROM system")
          } else {
            dbIOPSquery <- paste0("SELECT IOPS1,IOPS2,IOPS3,IOPS4,IOPS5,IOPS6,IOPS7,IOPS8,IOPS9 FROM dbsim WHERE Testname='",x,"' AND iteration=1")
            dbRTquery <- paste0("SELECT RT1,RT2,RT3,RT4,RT5,RT6,RT7,RT8,RT9 FROM dbsim WHERE Testname='",x,"' AND iteration=1")
            sysQuery <- paste0("SELECT Model, Ucode FROM system")
          }
          dbIOPSdata <- dbGetQuery(db, dbIOPSquery)
          dbRTdata <- dbGetQuery(db, dbRTquery)
          systemData <- dbGetQuery(db, sysQuery)
        }
      
        # put everything into a list so we can pass all the data to the next method
        dbList[[j]] <- list(nameData, dbIOPSdata, dbRTdata, systemData)
      }
      dbDisconnect(db)
      return(dbList)
    }
  }
  
  observeEvent(input$dbTable_rows_selected, {
    test <- input$dbTable_rows_selected

    if (is.null(test)) {
      # output nothing if nothing is selected
    }else{
      paths <<- globalDbdata[test, 'TestPath']
      numPaths <- NROW(paths)
      charData = charTestQuery()
      
      # render CHAR highchart
      output$charHC <- renderHighchart({
        hc <- highchart() %>%
          hc_yAxis(title = list(text = "IOps")) %>%
          # for (i in 1:numPaths) {
          #   hc_add_series(name = charData[[j]][[5]][1,1], charData[[j]][[3]][,4], type = "column")
          # }
          hc_add_series(name = charData[[1]][[5]][1,1], charData[[1]][[3]][,4], type = "column") %>%
          hc_add_series(name = charData[[2]][[5]][1,1], charData[[2]][[3]][,4], type = "column") %>%
          hc_add_series(name = charData[[3]][[5]][1,1], charData[[3]][[3]][,4], type = "column") %>%
          hc_xAxis(title = list(text = "Block Size"), label = list(text = "test"), categories = charData[[1]][[4]][,1]) %>%
          hc_title(
            text = input$charWorkload)%>%
          hc_tooltip(pointFormat = "
                     {series.name} IOps: <b>{point.y:,.0f}</b><br>
                     {series.name} MBps: <b>N/A</b>")
        
      })

      # render DBsim highchart
      dbData = dbTestQuery()
      iopsData <- NULL
      rtData <- NULL
      systemData <- NULL
      
      for (i in 1:numPaths) {
        # melt data to reform into columns so we can plot
        iopsData[[i]] <- melt(dbData[[i]][[2]])
        rtData[[i]] <- melt(dbData[[i]][[3]])
        systemData[[i]] <- melt(dbData[[i]][[4]])

        print(iopsData)
        print(rtData)
        print(systemData)
      }
      
      output$dbHC <- renderHighchart({
        hc <- highchart() %>%
          hc_add_series_scatter(name = systemData[[1]][1,1], iopsData[[1]][,2], rtData[[1]][,2], showInLegend = TRUE, lineWidth = 2) %>%
          hc_add_series_scatter(name = systemData[[2]][1,1], iopsData[[2]][,2], rtData[[2]][,2], showInLegend = TRUE, lineWidth = 2) %>%
          hc_add_series_scatter(name = systemData[[3]][1,1], iopsData[[3]][,2], rtData[[3]][,2], showInLegend = TRUE, lineWidth = 2) %>%
          hc_yAxis(title = list(text = "Response Time (ms)")) %>%
          hc_xAxis(title = list(text = "IOps")) %>%
          hc_title(
            text = input$dbWorkload)%>%
          hc_tooltip(pointFormat = "
                     {series.name} IOps: <b>{point.x:,.0f}</b><br>
                     {series.name} RT: <b>{point.y:,.2f}(ms)</b>")
      })
      
    }
    
  })
  
  observeEvent(input$charWorkload, {
    numPaths <- NROW(paths)
    charData = charTestQuery()
    
    # render CHAR highchart
    output$charHC <- renderHighchart({
      hc <- highchart() %>%
        hc_yAxis(title = list(text = "IOps")) %>%
        # for (i in 1:numPaths) {
        #   hc_add_series(name = charData[[j]][[5]][1,1], charData[[j]][[3]][,4], type = "column") %>%
        #   i = i + 1
        # }
        hc_add_series(name = charData[[1]][[5]][1,1], charData[[1]][[3]][,4], type = "column") %>%
        hc_add_series(name = charData[[2]][[5]][1,1], charData[[2]][[3]][,4], type = "column") %>%
        hc_add_series(name = charData[[3]][[5]][1,1], charData[[3]][[3]][,4], type = "column") %>%
        hc_xAxis(title = list(text = "Block Size"), label = list(text = "test"), categories = charData[[1]][[4]][,1]) %>%
        hc_title(
          text = input$charWorkload)%>%
        hc_tooltip(pointFormat = "
                   {series.name} IOps: <b>{point.y:,.0f}</b><br>
                   {series.name} MBps: <b>N/A</b>")
      
    })
    
  })
  
  observeEvent(input$dbWorkload, {
    numPaths <- NROW(paths)
    dbData = dbTestQuery()
    iopsData <- NULL
    rtData <- NULL
    systemData <- NULL

    if (numPaths == 0) {
      # forgrok
    }else{
      for (i in 1:numPaths) {
        # melt data to reform into columns so we can plot
        iopsData[[i]] <- melt(dbData[[i]][[2]])
        rtData[[i]] <- melt(dbData[[i]][[3]])
        systemData[[i]] <- melt(dbData[[i]][[4]])
  
        print(iopsData)
        print(rtData)
        print(systemData)
      }
  
      output$dbHC <- renderHighchart({
        hc <- highchart() %>%
          hc_add_series_scatter(name = systemData[[1]][1,1], iopsData[[1]][,2], rtData[[1]][,2], showInLegend = TRUE, lineWidth = 2) %>%
          hc_add_series_scatter(name = systemData[[2]][1,1], iopsData[[2]][,2], rtData[[2]][,2], showInLegend = TRUE, lineWidth = 2) %>%
          hc_add_series_scatter(name = systemData[[3]][1,1], iopsData[[3]][,2], rtData[[3]][,2], showInLegend = TRUE, lineWidth = 2) %>%
          hc_yAxis(title = list(text = "Response Time (ms)")) %>%
          hc_xAxis(title = list(text = "IOps")) %>%
          hc_title(
            text = input$dbWorkload)%>%
          hc_tooltip(pointFormat = "
                     {series.name} IOps: <b>{point.x:,.0f}</b><br>
                     {series.name} RT: <b>{point.y:,.2f}(ms)</b>")
      })
    }
  
  })
  
  # sidebarMenu triggers
  observeEvent(input$sidebarMenu, {
    if (input$sidebarMenu=="dashboard") {
      #output$catalogprint <- renderPrint({input$typeFilter})
      
    }
    
    if (input$sidebarMenu=="catalog") {
      
      output$list <- renderUI({
        tabBox(title = "OSPE Catalog", id = "tabset1", width = 12,
               tabPanel("Database", DT::dataTableOutput("dbTable")),
               tabPanel("Graphs",  fluidRow(column(4,selectInput("charWorkload", label = "CHAR Workload", choices = c("Random Read Hit", "Random Write Hit", "Random Max Write", "Random Sustained Write",
                                                                                           "Random Read Miss", "Sequential Write", "Sequential Read"), selected = NULL)),
                                   column(6,selectInput("dbWorkload", label = "DBsim Workload", choices = c("DSS128K", "OLTP2HW-IM", "OLTP2HW", "OLTP2_A", "RRH8K-IM", "RRM8K", "RWH512B-IM",
                                                                                           "RWH8K-IM", "RWM_8192", "SIZER1", "TERADATA_A"), selected = NULL))),
                                   highchartOutput("charHC"),highchartOutput("dbHC")),
               tabPanel("Test", verbatimTextOutput("zz")),
               tabPanel("Metadata", paste0("y")))
      })
      
      dbdata = dbFullQuery()
      globalDbdata <<- dbdata
      output$dbTable <- DT::renderDT({dbdata}, options = list(pageLength = 14, lengthChange = FALSE, autoWidth = TRUE, dom = 'tp', scrollX = TRUE, 
                                                              columnDefs = list(list(width = '150px', targets = c(1,2,3,4,5,6)))), 
                                     filter = list(position = 'top', clear = FALSE), rownames = FALSE)
    }
    
    if (input$sidebarMenu=="ospeboxes") {
      output$boxlist <- renderUI({
        box(title = "OSPE Boxes", status = "primary", width = 12,
            output$renderOSPEboxes <- DT::renderDT({
              location = "\\\\efsus03\\pdata\\OSPE_Group_Folder\\dashboard\\OSPerfMachineList.xlsx"
              read_xlsx(paste(location))
            }, options = list(pageLength = 5, lengthChange = FALSE))
        )
      })
    }
    
    if (input$sidebarMenu=="dellemcboxes") {
      getPage<-function(){
        return(includeHTML("\\\\efsus03\\pdata\\OSPE_Group_Folder\\dashboard\\Boxlist.htm"))
      }
      
      output$boxeshtml<-renderUI({
        box(title = "DellEMC Box Directory", status = "primary", width = 12, getPage())
      })
    }
    
    if (input$sidebarMenu=="bitsandbytes") {
      output$print <- renderPrint({"testo"})
    }
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
  
}

# Run app -----------------------------------------------------------
shinyApp(ui = ui, server = server)
