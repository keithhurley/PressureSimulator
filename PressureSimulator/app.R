library(shiny)
library(sf)
library(tidyverse)
source("../SimulationScripts/Functions_fish.R")
source("../SimulationScripts/Functions_angler.R")
source("../SimulationScripts/Functions_casts.R")
source("../SimulationScripts/Functions_themes.R")
library(tictoc)


# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage("Fishing Pressure Simulator",
                tabPanel("Settings",
                         navlistPanel(
                             tabPanel("Waterbody"),
                             tabPanel("Anglers",
                                      "Some Text Here"),
                             tabPanel("Fish",
                                      sliderInput("ipNumberFish", 
                                                  "Number of fish:",
                                                  min = 10,
                                                  max = 1000,
                                                  step = 5,
                                                  value = 100)), 
                             tabPanel("Pressure"),
                             tabPanel("Simulations",
                                      numericInput("ipSeed",
                                                   "Enter Seed:",
                                                   value=12345),
                                      sliderInput("ipNumberSimulations", 
                                                  "Number of simulation runs:",
                                                  min = 1,
                                                  max = 1000,
                                                  step = 10,
                                                  value = 1),
                                      actionButton("doSims", 
                                          "Run Simulations"))
                             )
                         ),
                tabPanel("Results"),
                tabPanel("Plots"#,
                         #plotOutput("distPlot")
                         ),
                tabPanel("Outputs")
               )               )

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$doSims,
                {
                    #RUN SIMULATION HERE
                    tic()
                    
                    showNotification("Simulations Run Now", duration=10, closeButton=FALSE)
                    
                    myRandomSeed=input$ipSeed
                    
                    showNotification("Loading Lake Geometries", duration=10, closeButton=FALSE)
                    
                    load(file="../data/lakes/round")
                    
                    showNotification("Placing Fish", duration=10, closeButton=FALSE)

                                        myFish<-fish_place_random(lakeGeom=lakes_round_base,
                                              numberFish=input$ipNumberFish,
                                              mySeed=input$ipSeed)
                    
                    showNotification("Placing Anglers", duration=10, closeButton=FALSE)
                    
                    myAnglers<-anglers_place(lakeGeom=lakes_round_base,
                                             totalAnglers = 1000,
                                             percentBank = 50,
                                             mySeed=input$ipSeed)
                    
                    showNotification("Simulating Casts", duration=10, closeButton=FALSE)

                    myCasts<-casts_place(lakeGeom=lakes_round_base,
                                         myAnglers, 
                                         numberCastsPerAngler=20,
                                         mySeed=input$ipSeed)

                    elapsedTime=toc()
                    showNotification(paste("Simulations Complete (Elapsed Time = ", round((elapsedTime$toc-elapsedTime$tic)/60,2), " Minutes)", sep=""), duration = NULL)
                    
                    print("Done")
                    
                    tmpFish<-st_intersects(st_buffer(myFish, 1), myCasts)
                    print(table(tmpFish %>% lengths) %>% data.frame() %>% rename("NumberInteractions"="Var1"))
                })
    
    #output$mySeed=reactive(input$ipSeed)
    #output$myNumberFish=reactive(input$ipNumberFish)
}

# Run the application 
shinyApp(ui = ui, server = server)
