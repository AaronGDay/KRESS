
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# server.R

# Libraries
library(shiny)
library(raster)
library(rasterVis)
library(rgdal)
library(rgl)
library(data.table)
library(reshape2)
library(ggplot2)
library(shinysky)

# Global Options 
rasterOptions(standardnames = FALSE)

# Initialize fileList to hold all uploaded files; each dimention is a different type
list1 <- list("")
list2 <- vector()
fileList <<- list(list1, stack(), list2)

# Load weedmapper file holding locations for plants in OREGON
plantDB <- fread("data/Pure_weedmapper_data.csv")
setkey(plantDB, Common_Name)

shinyServer(function(input, output) {
  
  # Home PAGE
   # No server.R material
  
  # Raster Tools (Map Algebra, Map Statistics, Plots) DROPDOW
   # Map Algebra PAGE
    output$LeftSide <- renderUI({
      # Select files for left side of algebra
      selectInput(
        inputId = "leftAlgebra",
        label = "Select Files",
        choices = input$AddMap[ ,1],
        multiple = TRUE
      )
    })
  
    output$RightSide <- renderUI({
      # Select files for right side of algebra
      selectInput(
        inputId = "rightAlgebra",
        label = "Select Files",
        choices = "suitability",
        multiple = TRUE
      )
    })
  
    output$testPlot <- renderPlot({
      # Shows the raster
      if(!is.null(suitability())){
        other <- suitability()
        fileList[[2]] <<- mask(fileList[[2]], other)
        plot(raster(fileList[[2]], layer = 1))
      }
    })
  
   # Map Statistics
    output$bar.plot <- renderPlot({
      # Displays histogram
      addRasterToList()
      if(!is.null(input$AddMap) && hasValues(toUse <- raster(fileList[[2]], layer = mapChoice()))){
        barplot(
          stretch(toUse),
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
  
    output$mapParam <- renderUI({
      # Displays map parameters
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
  
    output$mapStats <- renderText({
      # Displays map statistics
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
  
    output$SelectLayer <- renderUI({
      # Select option for which layer to display
      if(!is.null(input$AddMap)){
        selectInput(
          inputId = "LayerSelect", 
          label = "Select map to use", 
          choices = input$AddMap[, 1]
        )
      }
    })
    outputOptions(output, 'SelectLayer', suspendWhenHidden=FALSE)
  
   # Plots
     output$AllPlots <- renderPlot({
       # Plot all plots in the fileList in Plots page
       hist(stretch(fileList[[2]]))
     })
  
  # Model Evaluation
    output$FirstRaster <- renderPlot({
      # Plot the raster for Model Evaluation
      if(!is.null(input$SuitMap) && !is.null(input$YCoord)){
        toUseSuit <- raster(input$SuitMap[1, 4])
        plot(
          toUseSuit,
           xlab = input$XCoord,
           ylab = input$YCoord
        )
      }
    })
  
    output$GeneratedRaster <- renderPlot({
      # Plot the modified raster for Model Evaluation
      if(!is.null(input$PlantInput) && !is.null(input$SuitMap)){
        toUseSuit <- suitability()
        plot(
          toUseSuit,
          xlab = input$XCoord,
          ylab = input$YCoord,
          col = "black"
        )
      }
    })
  
    output$PlantTypeAhead <- renderUI({
      # Typeahead box for chosing which plant to use, in Model Evaluatoin
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
  
    output$XCoord <- renderUI({
      # Choose the XCoord variable in Map Eval page in datafile
      if(!is.null(input$PlantInput)){
        selectInput(
          inputId = "XCoord",
          selected = "Longitude_",
          label = "X Field:",
          choices = names(chosenPlant()),
          multiple = FALSE
        )
      }
    })
    
    output$YCoord <- renderUI({
      # Choose the YCoord variable in Map Eval page from datafile
      if(!is.null(input$PlantInput)){
        selectInput(
          inputId = "YCoord",
          selected = "Latitude_D",
          label = "Y Field:",
          choices = names(chosenPlant()),
          multiple = FALSE
        )
      }
    })
  
  # End menu items, functions below
  
  chosenPlant <- reactive({
    # CSV of locations of chosen plan for Model Evaluation
    if(!is.null(input$PlantInput)){
      plantLocations <- subset(plantDB, Common_Name == input$PlantInput)
    }
  })
  
  addRasterToList <- reactive({
    # Takes files uploaded and converts them to raster stack
    if(!is.null(input$AddMap)){
      for(i in 1:nrow(input$AddMap)){
        fileList[[2]] <<- addLayer(fileList[[2]], raster(input$AddMap[i,4]))
        fileList[[3]] <<- append(fileList[3], input$AddMap[i,1])
      }
    }
  })
  
  mapChoice <- reactive({
    # Variable for choice of map to display, reactive only to: LayerSelect, AddMap
    if(!is.null(input$AddMap)){
      for(j in 1:nrow(input$AddMap)){
        test1 <- input$LayerSelect
        test2 <- fileList[[3]]
        if(identical(input$LayerSelect, input$AddMap[j,1]) == TRUE){
            return(j)
        }
      }
    }
  })
  
  suitability <- reactive({
    # Returns a raster from CSV points provied by the weedmapper data of the selected plant
    if(!is.null(input$XCoord) && !is.null(input$YCoord) && !is.null(input$SuitMap)){
      theRaster <- raster(input$SuitMap[1, 4])
      theData <- as.data.table(chosenPlant())
      theData <- theData[ , c(input$XCoord, input$YCoord), with = FALSE]
      theRaster <- rasterize(theData, theRaster)
      return (as.logical(theRaster))
    }
  })
  
  dataFile <- reactive({
    # Reads file of the uploaded datafile, an alternative to the typeahead
    theCSV <- read.csv(input$DataFiles[1,4])
    return (theCSV)
  })
})
