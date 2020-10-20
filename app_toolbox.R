#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(knitr)
library(rgdal)
library(pander)
library(kableExtra)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(mapedit)
library(sf)
library(readr)
library(ncdf4)
library(ggplot2)
library(rasterVis)
library(papeR)
library(htmltools)
library(lwgeom)
library(shinyjs)

map <- leaflet() %>%
    addTiles()%>% setView(lng=-12,lat=53,zoom=6)%>%
    addDrawToolbar(polylineOptions = FALSE,circleOptions = FALSE,
                   markerOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   editOptions = editToolbarOptions()
    )%>%
addWMSTiles("https://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                layers = "0",
                options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"))

button_color_css <- "
 #DivCompClear, #FinderClear, #EnterTimes{
 /* Change the background color of the update button
 to blue. */
 background: DodgerBlue;
 /* Change the text size to 15 pixels. */
 font-size: 15px;
}"
# Define UI for application that draws a histogram
ui = fluidPage(theme = shinytheme("lumen"),
               useShinyjs(),
               options(shiny.sanitize.errors = TRUE),
               tags$head(tags$style(".shiny-notification{
                 position: fixed;
                 top: 33%;
                 left: 33%;
                 right: 33%;
                 background-color: lightgrey;
                  border: 1px solid #4CAF50;
                 
               }")),
               titlePanel("Use Case: Linking Ecosystem and Fisheries Data"),
               navbarPage(title = div(img(src="Niamh.png", height = 35,
                                          width = 500)),
                          
                          tabPanel("Data selection and Report Generation", fluid = TRUE,
                                   # Sidebar layout with a input and output definitions
                                   sidebarLayout(
                                       sidebarPanel(
                                           h4(HTML("<u> Step 1: Select Area of Interest </u>"))
                                           , verbatimTextOutput("ttN"),
                                           selectInput("select", "Select Coordinates:", choices = c("interactively","manually")),
                                           
                                           conditionalPanel(condition = "input.select == 'manually'",
                                                            
                                                            textInput('xcoord', 'Enter x coordinates (separated by comma)'),
                                                            textInput('ycoord', 'Enter y coordinates (separated by comma)')
                                                            ,
                                                            actionBttn(
                                                                inputId = "viewP",
                                                                label = "Validate selected polygon",
                                                                color = "success"
                                                            )),
                                           conditionalPanel(condition = "input.select == 'interactively'",
                                                            textInput('xcoord2', 'Enter x coordinates (separated by comma)'),
                                                            textInput('ycoord2', 'Enter y coordinates (separated by comma)')
                                           )
                                           ,
                                           h4(HTML("<u> Step 2: Data Sources </u>")),
                                           awesomeCheckboxGroup(
                                               inputId = "Data1", label="",
                                               choices = c("Seabed Habitats Mapping around Ireland"
                                               )),
                                           conditionalPanel(condition = "input.Data1.includes('Seabed Habitats Mapping around Ireland')",
                                                            prettyRadioButtons(inputId = "DS1",
                                                                                label = "", icon = icon("check"),
                                                                                choices = c("Biozone", "Substrate", "Folk_5","EUNIS","MSFD_BBHT"),
                                                                                animation = "tada", status = "default",inline = TRUE)),
                                           awesomeCheckboxGroup(
                                               inputId = "Data2", label="",
                                               choices = c(
                                                   "Bathymetry"
                                               )),
                                           
                                           conditionalPanel(condition = "input.Data2.includes('Bathymetry')",
                                                            prettyCheckboxGroup(inputId = "DS2",
                                                                                label = "", icon = icon("check"),
                                                                                choices = c("height_above_reference_ellipsoid"),
                                                                                selected="height_above_reference_ellipsoid",
                                                                                animation = "tada", status = "default",inline = TRUE)),
                                           awesomeCheckboxGroup(
                                               inputId = "Data3", label="",
                                               choices = c(
                                                   "North Atlantic Ocean Monthly Model Means"
                                               )),
                                           conditionalPanel(condition = "input.Data3.includes('North Atlantic Ocean Monthly Model Means')",
                                                            sliderInput("slider1", label = "Year", min = 2013, 
                                                                        max = 2019, value = c(2017, 2019),sep = ""),
                                                            selectizeInput('month1', 'Month', month.name, multiple=TRUE,selected = "January", options = list(maxItems = 3)),
                                                            prettyCheckboxGroup(inputId = "DS3",
                                                                                label = "", icon = icon("check"),
                                                                                choices =dput( as.vector(read.csv("Data/Roms.csv")[[2]])),
  
                                                                                animation = "tada", status = "default")),
                                           awesomeCheckboxGroup(
                                               inputId = "Data4", label="",
                                               choices = c(
                                                   "Copernicus Ocean Physics"
                                               )),
                                           conditionalPanel(condition = "input.Data4.includes('Copernicus Ocean Physics')",
                                                            sliderInput("slider2", label = "Year", min = 1993, 
                                                                        max = 2019, value = c(2016, 2019),sep = ""),
                                                            selectizeInput('month2', 'Month', month.name, multiple=TRUE,selected = "January", options = list(maxItems = 3) ),
                                                            prettyCheckboxGroup(inputId = "DS4",
                                                                                label = "", icon = icon("check"),
                                                                                choices = dput( as.vector(read.csv("Data/PhysDesc.csv")[[2]])),
                                                                                animation = "tada", status = "default",inline = TRUE)),
                                           awesomeCheckboxGroup(
                                               inputId = "Data5", label="",
                                               choices = c(
                                                   "Copernicus Ocean BioGeoChemistry"
                                                   
                                               )),
                                           conditionalPanel(condition = "input.Data5.includes('Copernicus Ocean BioGeoChemistry')",
                                                            sliderInput("slider3", label = "Year", min = 1992, 
                                                                        max = 2019, value = c(2016, 2019),sep = ""),
                                                            selectizeInput('month3', 'Month',month.name, multiple=TRUE,selected = "January", options = list(maxItems = 3) ),
                                                            prettyCheckboxGroup(inputId = "DS5",
                                                                                label = "", icon = icon("check"),
                                                                                choices = dput( as.vector(read.csv("Data/BioChemDesc.csv")[[2]])),
                                                                                animation = "tada", status = "default",inline = TRUE)),
                                           awesomeCheckboxGroup(
                                               inputId = "Data6", label="",
                                               choices = c(
                                                   "An operational zooplankton")
                                           ),
                                           conditionalPanel(condition = "input.Data6.includes('An operational zooplankton')",
                                                            sliderInput("slider4", label = "Year", min = 1958, 
                                                                        max = 2013, value = c(2011, 2013),sep = ""),
                                                            selectInput('season',"Quarter", c(Choose='',c("Q1","Q2","Q3","Q4")),selected = "Q1", selectize=FALSE),
                                                            prettyCheckboxGroup(inputId = "DS6",
                                                                                label = "", icon = icon("check"),
                                                                                choices = c("Acartia","Calanus_finmarchicus","Calanus_helgolandicus",
                                                                                            "Metridia_lucens","Temora_longicornis","Large_copepods","Small_copepods"),
                                                                                animation = "tada", status = "default",inline = TRUE),
                                                          
                                       selectInput('error',"Maximum Relative Error",c(0,0.3,0.5), selectize=FALSE)),
                                       awesomeCheckboxGroup(
                                         inputId = "Data7", label="",
                                         choices = c(
                                           "Waves"
                                         )),
                                       
                                       conditionalPanel(condition = "input.Data7.includes('Waves')",
                                                        prettyCheckboxGroup(inputId = "DS7",
                                                                            label = "", icon = icon("check"),
                                                                            choices = c("ke_waves_atlantic"),
                                                                            selected="ke_waves_atlantic",
                                                                            animation = "tada", status = "default",inline = TRUE)), 
                                       awesomeCheckboxGroup(
                                         inputId = "Data8", label="",
                                         choices = c(
                                           "Currents"
                                         )),
                                       
                                       conditionalPanel(condition = "input.Data8.includes('Currents')",
                                                        prettyCheckboxGroup(inputId = "DS8",
                                                                            label = "", icon = icon("check"),
                                                                            choices = c(" ke_currents_atlantic "),
                                                                            selected=" ke_currents_atlantic ",
                                                                            animation = "tada", status = "default",inline = TRUE)), 
                                           
                                           h4(HTML("<u> Step 3: Generate Report </u>")),
                                       #useShinyjs(),  # Set up shinyjs
                                           actionBttn(
                                               inputId = "report",
                                               label = "View and Save report",
                                               #color = "success",
                                               style = "fill",
                                               icon = icon("file-alt"),
                                               block = TRUE
                                           ) ,
                                       h4(HTML("<u> Step 4: Download Results </u><p> Press download if you are happy with selection in previous steps.</p>")),
                                       downloadButton("downloadResults", label = "Download")
                                       ),mainPanel(tabsetPanel( id="inTabset",
                                                                tabPanel("Area Selection Explorer",uiOutput("area"),uiOutput("action")),                      
                                                                tabPanel("Report",uiOutput("test")),tabPanel("Data Description",htmlOutput("dataD")) ))))))#uiOutput("dataD")#,


# Define server 
server = function(input, output,session) {
    
    
    
  
    
    
    
    
    
    output$action<-renderUI({
        if(input$select=="interactively"){
            actionBttn(
                inputId = "save",
                label = "Validate selected coordinates",
                color = "success"
            )
        }
        else {}
    })
    
    
    observeEvent(input$viewP,{
        
        updateTabsetPanel(session, "inTabset",selected ="Area Selection Explorer")
        xcoord<-as.numeric(unlist(strsplit(input$xcoord, ",")))
        ycoord<-as.numeric(unlist(strsplit(input$ycoord, ",")))
        if(length(which(xcoord<(-18)|xcoord>(-2)))>0){updateTextInput(session, "xcoord", value = "")}
        else(updateTextInput(session, "xcoord", value = xcoord))
        if(length(which(ycoord<(48)|ycoord>(56)))>0){updateTextInput(session, "ycoord", value = "")}
        else(updateTextInput(session, "ycoord", value = ycoord))
        if (!is.null(xcoord)) {
          if(length(which(xcoord<(-18)|xcoord>(-2)))>0){output$summaryx<-  renderText({"Choose xcoord between -18 and -2"})}
          else{ output$summaryx<-  renderText({xcoord})}}
        
        if (!is.null(ycoord)) {
          if(length(which(ycoord<(48)|ycoord>(56)))>0){output$summaryy<-  renderText({"Choose ycoord between 48 and 56"})}
          else{ output$summaryy<-  renderText({ycoord})}} 
        
        # output$summary <- renderText({
        #     
        #     return(paste(xcoord,ycoord))
        #     
        # })
        
        output$polP <- renderPlot({
          if(length(which(ycoord<(48)|ycoord>(56)))>0&length(which(xcoord<(-18)|xcoord>(-2)))>0){
            xym <- cbind(xcoord, ycoord)
            p = Polygon(xym)
            ps = Polygons(list(p),1)
            sps = SpatialPolygons(list(ps))
            proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
            plot(sps,axes=T)}
          else{}
            
        })
        output$area <- renderUI({
            
            list(verbatimTextOutput("summaryx"),verbatimTextOutput("summaryy"),plotOutput("polP"))
            
            
            
        })
    })
    
    
    
    
    
    
    observeEvent(input$select, { 
        updateTabsetPanel(session, "inTabset",selected ="Area Selection Explorer")
        if(input$select=="interactively"){
        
            edits <- callModule(
                editMod,
                leafmap = map,
                id = "map"
            )
            observeEvent(input$save, {
                updateTabsetPanel(session, "inTabset",selected ="Area Selection Explorer")
                x_geom <- edits()$finished$geometry[[1]][[1]][,1]
                y_geom<-edits()$finished$geometry[[1]][[1]][,2]
                if(length(which(x_geom<(-18)|x_geom>(-2)))>0){updateTextInput(session, "xcoord2", value = "")}
                else(updateTextInput(session, "xcoord2", value = x_geom))
                if(length(which(y_geom<(48)|y_geom>(56)))>0){updateTextInput(session, "ycoord2", value = "")}
                else(updateTextInput(session, "ycoord2", value = y_geom))
               if (!is.null(x_geom)) {
                 if(length(which(x_geom<(-18)|x_geom>(-2)))>0){output$xcoord<-  renderText({"Select xcoord between -18 and -2"})}
                   else{ output$xcoord<-  renderText({x_geom})}}
               
                if (!is.null(y_geom)) {
                  if(length(which(y_geom<(48)|y_geom>(56)))>0){output$ycoord<-  renderText({"Select ycoord between 48 and 56"})}
                  else{ output$ycoord<-  renderText({y_geom})}} 
                
               output$note<- renderText({"!!!After every selection click Recycling Bin to clear selected layer"})
               
            })
            output$area<-renderUI(
                list( editModUI("map"),
                      "x coordinates:",br(),
                      verbatimTextOutput("xcoord"),
                      "y coordinates:",br(),
                      verbatimTextOutput("ycoord"),
                      
                      verbatimTextOutput("note")
                ))}
        else{ 
            
            output$selectedX<-renderText(input$xcoord)
            output$selectedY<-renderText(input$ycoord)
            
            output$area <- renderUI({
            
            list(
                "x coordinates:",br(),
                textOutput("selectedX"),
                "y coordinates:",br(),
               textOutput("selectedY"))
               
                
                
            
        })}
    })
    
    observe({
      if(length(input$DS3) > 3){
        updatePrettyCheckboxGroup(session, "DS3", selected= "")
      }
      if(length(input$DS4) > 3){
        updatePrettyCheckboxGroup(session, "DS4", selected= "")
      }
      if(length(input$DS5) > 3){
        updatePrettyCheckboxGroup(session, "DS5", selected= "")
      }
      
      #   if(length(min(input$slider4):max(input$slider4)) > 20){
      #     updateSliderInput(session, "slider4", value = c(2011, 2013)) 
      #   
      # }
    })
    
       observe({
         if(input$select=="interactively"){
         if(input$xcoord2==""||input$ycoord2==""){
           disable("report")
           disable("downloadResults")
        }
         else{
        enable("report")
         enable("downloadResults")
         }}
         else if(input$select=="manually"){
           if(input$xcoord==""||input$ycoord==""){
             disable("report")
             disable("downloadResults")
           }
           else{
             enable("report")
             enable("downloadResults")
           }
         }
          
        })
   # output$ttN<-renderText(input$save==0 )
    
    observeEvent(input$report, {
        updateTabsetPanel(session, "inTabset",selected = "Report")
        ####################0.Select area of interest####
        #################################################
      
          if(input$select=="interactively"){
               params <- list(xcoord = input$xcoord2,
                              ycoord = input$ycoord2,
                             Data1=input$Data1,
                             DS1=input$DS1,
                             Data2=input$Data2,
                             DS2=input$DS2,
                             Data3=input$Data3,
                             DS3=input$DS3,
                             y1=input$slider1,
                             m1=input$month1,
                             Data4=input$Data4,
                             DS4=input$DS4,
                             y2=input$slider2,
                             m2=input$month2,
                             Data5=input$Data5,
                             DS5=input$DS5,
                             y3=input$slider3,
                             m3=input$month3,
                             Data6=input$Data6,
                             DS6=input$DS6,
                             y4=input$slider4,
                             season=input$season,
                             timeGap=input$ttZ,
                             error=input$error,
                             Data7=input$Data7,
                             DS7=input$DS7,
                             Data8=input$Data8,
                             DS8=input$DS8,
                             rendered_by_shiny = TRUE
                           )
          }
          else if (input$select=="manually"){
             params <- list(xcoord = input$xcoord,
                             ycoord = input$ycoord,
                            Data1=input$Data1,
                            DS1=input$DS1,
                            Data2=input$Data2,
                            DS2=input$DS2,
                            Data3=input$Data3,
                            DS3=input$DS3,
                            y1=input$slider1,
                            m1=input$month1,
                            Data4=input$Data4,
                            DS4=input$DS4,
                            y2=input$slider2,
                            m2=input$month2,
                            Data5=input$Data5,
                            DS5=input$DS5,
                            y3=input$slider3,
                            m3=input$month3,
                            Data6=input$Data6,
                            DS6=input$DS6,
                            y4=input$slider4,
                            Data7=input$Data7,
                            DS7=input$DS7,
                            Data8=input$Data8,
                            DS8=input$DS8,
                            season=input$season,
                            timeGap=input$ttZ,
                            error=input$error,
                            rendered_by_shiny = TRUE
                          
                           )  
        }
         
        
        output$test <- renderUI({
      list( withProgress(message = 'Generating Report',includeHTML(rmarkdown::render("report.Rmd",
                                               params = params,output_dir = paste0(getwd(),"/results")
                 ))))
            
        })
    })
    source("functions/combine.R")
    
     output$dataD <- renderUI({
      htmltools::includeHTML("Sources.html")
       }) 
 
    
   ####Download results####### 
    
    output$downloadResults <- downloadHandler(
      filename <- function() {
        paste("output", "zip", sep=".")
      },
      
      content <- function(file) {
        zip(file, "results/")
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
