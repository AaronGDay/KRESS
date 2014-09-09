
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#ui.R

library(shiny)
library(raster)
library(rasterVis)
library(ggplot2)

shinyUI(
  navbarPage("KRESS",
             tabPanel("Home", 
                      sidebarLayout(
                        sidebarPanel(
                          paste("Welcome to KRESS")
                        ),
                        mainPanel(
                          helpText("KRESS Modeler")
                          )
                      )
             ),
             navbarMenu("Raster Tools",
                        tabPanel("Map Algebra", 
                                 fluidPage(
                                   h4("Map Algebra"),
                                   hr(),
                                   column(3,
                                          title = "Left Side Algebra",
                                          htmlOutput("LeftSide"),
                                          br(),
                                          br(),
                                          fileInput(inputId = "AddMap1",
                                                    label = "Add Files",
                                                    multiple = TRUE,
                                                    accept = NULL),
                                          offset = 0
                                   ),
                                   column(1,
                                          title = "Algebra Functions",
                                          br(),
                                          radioButtons(inputId = "MathOptions",
                                                       label = NULL,
                                                       choices = list("+" = 1, 
                                                                      "-" = 2, 
                                                                      "x" = 3,
                                                                      "÷" = 4,
                                                                      "^" = 5)
                                          ),
                                          actionButton(inputId = "ProcessAlgebra",
                                                       label = "Process"
                                          )
                                   ),
                                   column(3,
                                          title = "Right Side Algebra",
                                          htmlOutput("RightSide"),
                                          br(),
                                          br(),
                                          fileInput(inputId = "AddMap1",
                                                    label = "Add Files",
                                                    multiple = TRUE,
                                                    accept = NULL
                                          )
                                   ),
                                   column(5,
                                     plotOutput("testPlot"),
                                     br(),
                                     br()
                                   )
                                 ) #End fluid Row w/ all algebra
                        ), #End tabPanel w/ Map Algebra
                        tabPanel("Map Statistics",
                                 fluidPage(
                                   title = "Map Statistics",
                                   h4("Map Statistics"),
                                   hr(),
                                   plotOutput("bar.plot"),
                                   hr(),
                                   column(2, 
                                          fileInput(inputId = "AddMap",
                                                    label = "Add Map(s)",
                                                    multiple = TRUE,
                                                    accept = NULL),
                                          fileInput(inputId = "AddMask",
                                                    label = "Add Mask",
                                                    multiple = FALSE,
                                                    accept = NULL
                                          ),
                                          checkboxInput("useMask", "Apply Mask")
                                   ),
                                   column(3,
                                          h5("Map Parameters:"),
                                          htmlOutput("mapParam"),
                                          offset = 1
                                   ),
                                   column(3,
                                          h5("Map Statistics:"),
                                          htmlOutput("mapStats")
                                   ),
                                   column(3, 
                                          numericInput("Weight", 
                                                       label = "Weight", 
                                                       value = 1, 
                                                       min = 1, 
                                                       max = 4
                                          ),
                                          htmlOutput("SelectLayer")
                                   )
                                 ) #End FluidPage w/ Map Statistics
                        ), #End tabPanel w/ Map Statistics
                        tabPanel("Plots",
                                 fluidPage(
                                   title = "Plots",
                                   column(12,
                                   plotOutput("AllPlots")
                                   )
                                   )
                                 )
             ), #End navBarPage w/ Raster Tools
             tabPanel("Model Evaluation",
                      fluidPage(
                        h4("Model Evaluation"),
                        hr(),
                        column(12,
                               fluidRow(
                                 column(6,
                                        plotOutput("FirstRaster")
                                 ),
                                 column(6,
                                        plotOutput("GeneratedRaster")
                                 ),
                                 fluidRow(
                                   column(2,
                                          fileInput(inputId = "SuitMap",
                                                    label = "Add Suitability Map",
                                                    multiple = FALSE,
                                                    accept = NULL),
                                          fileInput(inputId = "SuitMask",
                                                    label = "Add Mask",
                                                    multiple = FALSE,
                                                    accept = NULL),
                                          checkboxInput("useSuitMask", "Apply Mask"),
                                          offset = 1
                                   ),
                                   column(2,
#                                           fileInput(inputId = "DataFiles",
#                                                     label = "Data Files",
#                                                     multiple = FALSE,
#                                                     accept = NULL
#                                                     ),
                                          HTML("OR"),
                                          htmlOutput("PlantTypeAhead"),
                                          htmlOutput("XCoord"),
                                          htmlOutput("YCoord"),
                                          br(),
                                          br(),
                                          br(),
                                          offset = 2
                                   ),
                                   column(3#,
                                          #submitButton(text = "Generate"
                                          #             )
                                          )
                                 ) #End Fluid row w/ file inputs
                               ) # End Fluid row w/plots
                        ) #End Column 12 w/ empty
                      ) #End Fluid row 
             ) #End tabPanel Model Evaluation
  ) #End navBarPage
) #End ShinyUI