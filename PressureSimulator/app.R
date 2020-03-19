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
               id="PageNav",
                tabPanel("Settings",
                         navlistPanel(
                             tabPanel("Waterbody",
                                      selectInput('ipLakeGeom',
                                                  'Select A Lake',
                                                  choice = gsub("\\.rData$", "", list.files('../data/lakes/'))
                                                  ),
                                      tags$b("Lake Name: "),
                                      textOutput("opLakeName"),
                                      tags$br(),
                                      tags$b("Acres: "),
                                      textOutput("opLakeAcres")
                             ),
                             tabPanel("Anglers",
                                      selectInput("ipAnglerDistributionType",
                                                  "Distribution Type",
                                                  choices=c("Random", 
                                                            "Clustered By Party"),
                                                  selected="Clustered By Party"),
                                      conditionalPanel("input.ipAnglerDistributionType == 'Clustered By Party'",
                                                       tags$span(style="font-weight:bold",
                                                                 "Clustered By Party Options: "),
                                                       tags$div(style="background: gainsboro; padding:10px, margin-bottom:10px;",
                                                                sliderInput("ipBoatAnglerPartyClusterRadius",
                                                                            "Boat Angler Party Radius",
                                                                            min=1,
                                                                            max=5,
                                                                            step=1,
                                                                            value=2.5),
                                                                sliderInput("ipBankAnglerPartyClusterRadius",
                                                                            "Bank Angler Party Radius For Two Anglers (will automatically adjust for party size)",
                                                                            min=1,
                                                                            max=15,
                                                                            step=1,
                                                                            value=5)
                                                                )
                                                       ),
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
                                                  value = 4),
                                      sliderInput("ipBoatShorelineBuffer", 
                                                  "Boating Shoreline Buffer: ",
                                                  min = 0.5,
                                                  max = 30,
                                                  step = 0.5,
                                                  value = 10)),
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
                                                  value = 100),
                                      sliderInput("ipFishShorelineBuffer", 
                                                "Shoreline Buffer: ",
                                                min = 0.5,
                                                max = 2,
                                                step = 0.5,
                                                value = 0.5)),
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
                tabPanel("Results",
                         tableOutput('TableInteractions')),
                tabPanel("Plots"#,
                         #plotOutput("distPlot")
                         ),
                tabPanel("Outputs")
               )               )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    myValues<-reactiveValues()
    
    
    #setup lake name and acres display
    observeEvent(input$ipLakeGeom,
                 {
                     #load(file="./data/lakes/round_1.rData")
                     load(file=paste("../data/lakes/", input$ipLakeGeom, ".rData", sep=""))
                     myValues$lakeName=lake$name[1]
                     myValues$lakeAcres=as.numeric(round(st_area(lake)/4046.86,1))
                 })
    
    output$opLakeName=renderText(myValues$lakeName)
    
    output$opLakeAcres=renderText(myValues$lakeAcres)
    
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
    
    observeEvent(input$doSims,
                 {
                   updateTabsetPanel(session, "PageNav",
                                      selected = "Results")  
                 })

    #run Simulations
    SimsResult<-eventReactive(input$doSims,
                {
                    
                    
                    #RUN SIMULATION HERE
                    tic()
                    
                    showNotification("Simulations Run Now", duration=10, closeButton=FALSE)
                    
                    myRandomSeed=input$ipSeed
                    
                    showNotification("Loading Lake Geometries", duration=10, closeButton=FALSE)
                    
                    showNotification("Placing Fish", duration=10, closeButton=FALSE)

                    myFish<-fish_place_random(lakeGeom=lake,
                        numberFish=input$ipNumberFish,
                        fishShorelineBuffer = input$ipFishShorelineBuffer,
                        mySeed=input$ipSeed)
                
                    showNotification("Placing Anglers", duration=10, closeButton=FALSE)
                    
                    myAnglers<-anglers_place(lakeGeom=lake,
                                             anglerBoatDistribution = input$ipAnglerDistributionType,
                                             anglerBankDistribution = input$ipAnglerDistributionType,
                                             anglerBoatPartyRadius = input$ipBoatAnglerPartyRadius,
                                             anglerBankPartyRadius = input$ipBankAnglerPartyRadius,
                                             totalAnglers = 1000,
                                             meanPartySizeBoat=input$ipMeanPartySizeBoat,
                                             maxPartySizeBoat=input$ipMaxPartySizeBoat,
                                             meanPartySizeBank=input$ipMeanPartySizeBank,
                                             maxPartySizeBank=input$ipMaxPartySizeBank,
                                             boatShorelineBuffer= input$ipBoatShorelineBuffer,
                                             percentBank = input$ipPercentBank,
                                             mySeed=input$ipSeed)
                    
                    print(ggplot() +
                       geom_sf(data=lake, fill="lightskyblue") +
                       geom_sf(data=myAnglers, color="red", size=2) +
                       #geom_sf(data=myFish, color="black", size=1.5) +
                       labs(title="Anglers"))
                    
                    showNotification("Simulating Casts", duration=10, closeButton=FALSE)

                    myCasts<-casts_place(lakeGeom=lake,
                                         myAnglers, 
                                         numberCastsPerAngler=20,
                                         mySeed=input$ipSeed)

                    elapsedTime=toc()
                    showNotification(paste("Simulations Complete (Elapsed Time = ", round((elapsedTime$toc-elapsedTime$tic)/60,2), " Minutes)", sep=""), duration = NULL)
                    
                    print("Done")
                    
                    tmpFish<-st_intersects(st_buffer(myFish, 1), myCasts)
                    tblInteractionTable<-table(tmpFish %>% lengths) %>% data.frame() %>% rename("NumberInteractions"="Var1")
                    print(tblInteractionTable)
                    #outputTableInteractions=table(tmpFish %>% lengths) %>% data.frame() %>% rename("NumberInteractions"="Var1")
                    return(tblInteractionTable)
                })
    
    output$TableInteractions=renderTable({SimsResult()})
    #output$mySeed=reactive(input$ipSeed)
    #output$myNumberFish=reactive(input$ipNumberFish)
}

# Run the application 
shinyApp(ui = ui, server = server)
