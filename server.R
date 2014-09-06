
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#server.R

library(shiny)
library(raster)
library(rasterVis)
library(rgdal)
library(rgl)
library(data.table)
library(reshape2)
library(ggplot2)
library(shinysky)

rasterOptions(standardnames = FALSE)
list1 <- list("")
list2 <- vector()
fileList <<- list(list1, stack(), list2)

plantDB <- fread("data/Pure_weedmapper_data.csv")
setkey(plantDB, Common_Name)

shinyServer(function(input, output) {
  
  # Home PAGE
   # No server.R material
  
  # Raster Tools (Map Algebra, Map Statistics, Plots) DROPDOW
   # Map Algebra PAGE
    # Select files for left side of algebra
    output$LeftSide <- renderUI({
      selectInput(inputId = "leftAlgebra",
                  label = "Select Files",
                  choices = input$AddMap[ ,1],
                  multiple = TRUE)
    })
  
    # Select files for right side of algebra
    output$RightSide <- renderUI({
      selectInput(inputId = "rightAlgebra",
                  label = "Select Files",
                  choices = "suitability",
                  multiple = TRUE)
    })
  
    # Shows the raster
    output$testPlot <- renderPlot({
      if(!is.null(suitability())){
        other <- suitability()
        fileList[[2]] <<- mask(fileList[[2]], other)
        plot(raster(fileList[[2]], layer = 1))
      }
    })
  
   # Map Statistics
    # Displays histogram
    output$bar.plot <- renderPlot({
      addRasterToList()
      if(!is.null(input$AddMap) && hasValues(toUse <- raster(fileList[[2]], layer = mapChoice()))){
        barplot(stretch(toUse),
                border = TRUE,
                # main = title,
                xlab = "Value",
                ylab = "Frequency",
                col = "black",
                # digits = NULL,
                breaks = 255
        )
      }
    })
  
    # Displays map parameters
    output$mapParam <- renderUI({
      if(!is.null(input$AddMap) && hasValues(map <- raster(fileList[[2]], layer = mapChoice()))){
        map.columns <- paste("Columns: ", ncol(map))
        map.rows <- paste("Rows: ", nrow(map))
        map.x.lower.left <- paste("X Lower Left: ", xmin(map))
        map.y.lower.left <- paste("Y Lower Left: ", ymin(map))
        map.cell.size <- paste("Cell Size: ", xres(map))
        
        HTML(paste(map.columns, map.rows, map.x.lower.left, map.y.lower.left, map.cell.size, sep = '<br/>'))
      }
      else
        HTML(paste("Columns:", "Rows:", "X Lower Left:", "Y Lower Left:", "Cell Size:", sep = '<br/>'))
    })
  
    # Displays map statistics
    output$mapStats <- renderText({
      if(!is.null(input$AddMap) && hasValues(map <- raster(fileList[[2]], layer = mapChoice()))){
        map.min <- paste("Minimum: ", prettyNum(cellStats(map, "min"), drop0trailinng = TRUE, digits = 6))
        map.max <- paste("Maximum: ", prettyNum(cellStats(map, "max"), drop0trailinng = TRUE, digits = 6))
        map.mean <- paste("Mean: ", prettyNum(cellStats(map, "mean"), drop0trailinng = TRUE, digits = 8))
        map.sd <- paste("Standard Deviation: ", prettyNum(cellStats(map, "sd"), drop0trailing = TRUE, digits = 8))
        map.cell.count <- paste("Cell Count: ", ncell(map)-cellStats(map, "countNA"))
        map.cell.sum <- paste("Cell Sum: ", prettyNum(cellStats(map, "sum"),drop0trailinng = TRUE, digits = 8))
        
        HTML(paste(map.min, map.max, map.mean, map.sd, map.cell.count, map.cell.sum, sep = '<br/>'))
      }
      else
        HTML(paste("Minimum:", "Maximum:", "Mean:", "Standard Deviation:", "Cell Count:", "Cell Sum:", sep = '<br/>'))
    })
  
    # Select option for which layer to display
    output$SelectLayer <- renderUI({
      if(!is.null(input$AddMap)){
        selectInput(inputId = "LayerSelect", 
                    label = "Select map to use", 
                    choices = input$AddMap[, 1])
      }
    })
    outputOptions(output, 'SelectLayer', suspendWhenHidden=FALSE)
  
   # Plots
     # Plot all plots in the fileList in Plots page
     output$AllPlots <- renderPlot({
       hist(stretch(fileList[[2]]))
     })
  
  # Model Evaluation
    # Plot the raster for Model Evaluation
    output$FirstRaster <- renderPlot({
      if(!is.null(input$SuitMap) && !is.null(input$YCoord)){
        toUseSuit <- raster(input$SuitMap[1, 4])
        plot(toUseSuit,
             xlab = input$XCoord,
             ylab = input$YCoord
        )
      }
    })
  
    #Plot the modified raster for Model Evaluation
    output$GeneratedRaster <- renderPlot({
      if(!is.null(input$DataFiles)){
        toUseSuit <- suitability()
        plot(toUseSuit,
             xlab = input$XCoord,
             ylab = input$YCoord,
             col = "black"
        )
      }
    })
  
    #Typeahead box for chosing which plant to use, in Model Evaluatoin
    output$PlantTypeAhead <- renderUI({
      textInput.typeahead(
        id = "PlantInput",
        placeholder = "Plant common name",
        local = unique(plantDB),
        valueKey = 'Common_Name',
        tokens = c(1:nrow(plantDB)),
        limit = 3,
        template = HTML("<p class='repo-language'>{{USDA_Code}}</p><p class='repo-name'>{{Common_Name}}</p><p class='repo-description'>{{Genus_Species}}</p>")
      )
    })
  
    # Choose the XCoord variable in Map Eval page in datafile
    output$XCoord <- renderUI({
      if(!is.null(input$DataFiles)){
        selectInput(inputId = "XCoord",
                    label = "X Field:",
                    choices = names(dataFile()),
                    multiple = FALSE
        )
      }
    })
    
    # Choose the YCoord variable in Map Eval page from datafile
    output$YCoord <- renderUI({
      if(!is.null(input$DataFiles)){
        selectInput(inputId = "YCoord",
                    label = "Y Field:",
                    choices = names(dataFile()),
                    multiple = FALSE
        )
      }
    })
  
  #End menu items
  
  #Takes files uploaded and converts them to raster stack
  addRasterToList <- reactive({
    if(!is.null(input$AddMap)){
      for(i in 1:nrow(input$AddMap)){
        fileList[[2]] <<- addLayer(fileList[[2]], raster(input$AddMap[i,4]))
        fileList[[3]] <<- append(fileList[3], input$AddMap[i,1])
      }
    }
  })
  
#   addFileToList <- function(x){
#     fileList[1] <- cbind(fileList[1], x)
#   }
  
#   #Variable for the raster stack, reactive only to: readRaster, useMask, Weight
#   mapInput <- reactive({
#     if(!is.null(input$AddMap)){
#       if(input$useMask == TRUE && !is.null(input$AddMask) == TRUE){
#         mask(fileList[2], raster(input$AddMask[1,4]))
#       }
#       weight <- input$Weight
#       #theStack <- calc(theStack, fun = function(x){x*weight})
#       return (theRaster)
#     }
#   })
  
  #Variable for choice of map to display, reactive only to: LayerSelect, AddMap
  mapChoice <- reactive({
    if(!is.null(input$AddMap)){
#       for(j in 1:nlayers(fileList[[2]])){
        for(j in 1:nrow(input$AddMap)){
          test1 <- input$LayerSelect
          test2 <- fileList[[3]]
        if(identical(input$LayerSelect, input$AddMap[j,1]) == TRUE){
        #if(isTRUE(all.equal(input$LayerSelect, raster(fileList[[2]], layer = j), use.names = TRUE))){
            return(j)
        }
      }
    }
  })
  
  suitability <- reactive({
    if(!is.null(input$XCoord) && !is.null(input$YCoord)){
      theRaster <- raster(input$SuitMap[1, 4])
      theData <- as.data.table(dataFile())
      theData <- theData[ , c(input$XCoord, input$YCoord), with = FALSE]
      theRaster <- rasterize(theData, theRaster)
      theRaster <- as.logical(theRaster)
      return (theRaster)
    }
  })
  
  dataFile <- reactive({
    theCSV <- read.csv(input$DataFiles[1,4])
    return (theCSV)
  })




})
