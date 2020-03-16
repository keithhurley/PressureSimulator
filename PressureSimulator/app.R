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
                                      selectInput("ipAnglerDistributionType",
                                                  "Distribution Type",
                                                  choices=c("Random", 
                                                            "Clustered By Party"),
                                                  selected="Random"),
                                      tags$span(style="font-weight:bold",
                                                "Boat:"),
                                      tags$div(style="background: gainsboro; padding:10px",
                                      sliderInput("ipPercentBoat",
                                                  "Percent Boat Anglers",
                                                  min=0,
                                                  max=100,
                                                  step=5,
                                                  value=50),
                                      sliderInput("ipMeanPartySizeBoat", 
                                                  "Mean Party Size - Boat",
                                                  min = 1,
                                                  max = 10,
                                                  step = 0.1,
                                                  value = 1.9),
                                      sliderInput("ipMaxPartySizeBoat", 
                                                  "Mean Party Size - Boat",
                                                  min = 1,
                                                  max = 10,
                                                  step = 1,
                                                  value = 4)),
                                      tags$br(),
                                      tags$span(style="font-weight:bold",
                                                "Bank:"),
                                      tags$div(style="background: gainsboro; padding:10px",
                                               sliderInput("ipMeanPartySizeBank", 
                                                  "Mean Party Size - Bank",
                                                  min = 1,
                                                  max = 10,
                                                  step = 0.1,
                                                  value = 2.3),
                                      sliderInput("ipMaxPartySizeBank", 
                                                  "Mean Party Size - Bank",
                                                  min = 1,
                                                  max = 10,
                                                  step = 1,
                                                  value = 4),
                                      sliderInput("ipPercentBank",
                                                  "Percent Bank Anglers",
                                                  min=0,
                                                  max=100,
                                                  step=5,
                                                  value=50))),
                             tabPanel("Fish",
                                      sliderInput("ipNumberFish", 
                                                  "Number of fish:",
                                                  min = 10,
                                                  max = 1000,
                                                  step = 5,
                                                  value = 100)), 
                             tabPanel("Pressure",
                                      sliderInput("ipHoursPerAcre", 
                                                  "Hours Per Acre:",
                                                  min = 5,
                                                  max = 1000,
                                                  step = 5,
                                                  value = 100),
                                      sliderInput("ipMeanTripLength", 
                                                  "Mean Trip Length",
                                                  min = 0.5,
                                                  max = 10,
                                                  step = 0.5,
                                                  value = 3),
                                      sliderInput("ipCastsPerHour", 
                                                  "Casts Per Hour",
                                                  min = 4,
                                                  max = 120,
                                                  step = 1,
                                                  value = 60)),
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
server <- function(input, output, session) {
    
    #adjust boat/bank sliders to equal 100
    observeEvent(input$ipPercentBoat,
                 {
                     updateSliderInput(session,
                                       "ipPercentBank", 
                                       value=100-input$ipPercentBoat)
                 })
    observeEvent(input$ipPercentBank,
                 {
                     updateSliderInput(session,
                                       "ipPercentBoat", 
                                       value=100-input$ipPercentBank)
                 })
    
    #run Simulations
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
                                             anglerBoatDistribution = input$ipAnglerDistributionType,
                                             anglerBankDistribution = input$ipAnglerDistributionType,
                                             totalAnglers = 1000,
                                             meanPartySizeBoat=input$ipMeanPartySizeBoat,
                                             maxPartySizeBoat=input$ipMaxPartySizeBoat,
                                             meanPartySizeBank=input$ipMeanPartySizeBank,
                                             maxPartySizeBank=input$ipMaxPartySizeBank,
                                             percentBank = input$ipPercentBank,
                                             mySeed=input$ipSeed)
                    
                    print(ggplot() +
                       geom_sf(data=lakes_round_base, fill="lightskyblue") +
                       geom_sf(data=myAnglers, color="red", size=2) +
                       #geom_sf(data=myFish, color="black", size=1.5) +
                       labs(title="Anglers"))
                    
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
