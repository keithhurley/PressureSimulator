library(shiny)
library(sf)
library(tidyverse)
source("../SimulationScripts/Functions_fish.R")
source("../SimulationScripts/Functions_angler.R")
source("../SimulationScripts/Functions_casts.R")
source("../SimulationScripts/Functions_pressure.R")
source("../SimulationScripts/Functions_themes.R")
library(tictoc)

# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage("Fishing Pressure Simulator",
               id="PageNav",
                tabPanel("Settings",
                         navlistPanel(
                             tabPanel("Waterbody",
                                      tags$table(
                                          tags$tr(
                                              tags$td(
                                                  tags$b("Lake Name: "),
                                                  textOutput("opLakeName")
                                                  ),
                                              tags$td(style="width:25px;"),
                                              tags$td(
                                                  tags$b("Acres: "),
                                                  textOutput("opLakeAcres")
                                                  )
                                              ),
                                      selectInput('ipLakeGeom',
                                                  'Select A Lake',
                                                  choice = list.dirs(path='../data/lakes/',recursive=FALSE, full.names = FALSE)
                                                  ),
                                      uiOutput("restrictionSelection")
                                      )
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
                                      tabsetPanel(type="tabs",
                                                  tabPanel("Boat Anglers: ",
                                                        tags$div(style="background: gainsboro; padding:10px",
                                                            sliderInput("ipPercentBoat",
                                                            "Percent Boat Anglers",
                                                            min=0,
                                                            max=100,
                                                            step=5,
                                                            value=50),
                                                        sliderInput("ipMeanPartySizeBoat", 
                                                            "Mean Party Size",
                                                            min = 1,
                                                            max = 10,
                                                            step = 0.1,
                                                            value = 1.9),
                                                        sliderInput("ipMaxPartySizeBoat", 
                                                            "Max Party Size",
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
                                                        tags$br()),
                                                  tabPanel("Bank Anglers:  ",
                                                            tags$div(style="background: gainsboro; padding:10px",
                                                            sliderInput("ipPercentBank",
                                                                "Percent Bank Anglers",
                                                                min=0,
                                                                max=100,
                                                                step=5,
                                                                value=50),
                                                            sliderInput("ipMeanPartySizeBank", 
                                                                "Mean Party Size",
                                                                min = 1,
                                                                max = 10,
                                                                step = 0.1,
                                                                value = 2.3),
                                                            sliderInput("ipMaxPartySizeBank", 
                                                                "Max Party Size",
                                                                min = 1,
                                                                max = 10,
                                                                step = 1,
                                                                value = 4)
                                                            )))),
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
                                                value = 0.5),
                                      sliderInput("ipDetectionDistance",
                                                  "Detection Distance: ",
                                                  min=0.25,
                                                  max=10,
                                                  step=0.25,
                                                  value=1)),
                             tabPanel("Pressure",
                                      sliderInput("ipHoursPerAcre", 
                                                  "Hours Per Acre:",
                                                  min = 5,
                                                  max = 1000,
                                                  step = 5,
                                                  value = 100),
                                      sliderInput("ipTripLengthMean", 
                                                  "Trip Length Mean",
                                                  min = 0.5,
                                                  max = 10,
                                                  step = 0.5,
                                                  value = 3),
                                      sliderInput("ipTripLengthSd", 
                                                  "Trip Length Standard Deviation",
                                                  min = 0.5,
                                                  max = 10,
                                                  step = 0.5,
                                                  value = 1),
                                      sliderInput("ipCastsPerHourMean", 
                                                  "Casts Per Hour Mean",
                                                  min = 4,
                                                  max = 120,
                                                  step = 1,
                                                  value = 60),
                                      sliderInput("ipCastsPerHourSd", 
                                                  "Casts Per Hour Standard Deviation",
                                                  min = 4,
                                                  max = 120,
                                                  step = 1,
                                                  value = 60),
                                      sliderInput("ipCastDistanceMean",
                                                  "Mean Cast Distance",
                                                  min=1,
                                                  max=30,
                                                  step=0.5,
                                                  value=10),
                                      sliderInput("ipCastDistanceSd",
                                                  "Cast Distance Standard Deviation: ",
                                                  min=0.5,
                                                  max=5,
                                                  step=0.5,
                                                  value=3)),
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
                         tags$div(style="max-width:700px;",
                                  tags$div(style="float:left; width:200px;",
                                           tableOutput('TableInteractions')),
                                  tags$div(style="float:left; width:500px; padding-left:50px;",
                                           plotOutput('PlotInteractions'))
                                )
                         ),
                tabPanel("Plots"#,
                         #plotOutput("distPlot")
                         ),
                tabPanel("Outputs")
               )               
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    myValues<-reactiveValues()
    
    
    #setup lake name and acres display
    observeEvent(input$ipLakeGeom,
                 {
                     #load(file="./data/lakes/round_1/lake.rData")
                     load(file=paste("../data/lakes/", input$ipLakeGeom, "/lake.rData", sep=""))
                     myValues$lake=lake
                     myValues$lakeName=lake$name[1]
                     myValues$lakeAcres=as.numeric(round(st_area(lake)/4046.86,1))
                 })
    
    output$restrictionSelection <- renderUI({
        selectInput("ipShoreRestrictions", 
                    "Select A Shoreline Restriction: ",
                    choice = c("None", gsub("//.rData", "", list.files(path=paste("../data/lakes/",input$ipLakeGeom,"/restrictions/shore/", sep=""), pattern=".rData", recursive=FALSE, full.names = FALSE))))
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
                    
                    myValues$myPressureObject<-createPressureObject(myValues$lakeAcres,
                                                           input$ipHoursPerAcre,
                                                           input$ipTripLengthMean,
                                                           input$ipTripLengthSd,
                                                           input$ipCastsPerHourMean,
                                                           input$ipCastsPerHourSd)
                    print(myValues$myPressureObject)
                    showNotification("Loading Lake Geometries", duration=10, closeButton=FALSE)
                    
                    showNotification("Placing Fish", duration=10, closeButton=FALSE)

                    myFish<-fish_place_random(lakeGeom=myValues$lake,
                        numberFish=input$ipNumberFish,
                        fishShorelineBuffer = input$ipFishShorelineBuffer,
                        mySeed=input$ipSeed)
                
                    showNotification("Placing Anglers", duration=10, closeButton=FALSE)
                    suppressWarnings(
                    myAnglers<-anglers_place(lakeGeom=myValues$lake,
                                             anglerBoatDistribution = input$ipAnglerDistributionType,
                                             anglerBankDistribution = input$ipAnglerDistributionType,
                                             anglerBoatPartyRadius = input$ipBoatAnglerPartyClusterRadius,
                                             anglerBankPartyRadius = input$ipBankAnglerPartyClusterRadius,
                                             totalAnglers = myValues$myPressureObject$myNumberAnglers,
                                             meanPartySizeBoat=input$ipMeanPartySizeBoat,
                                             maxPartySizeBoat=input$ipMaxPartySizeBoat,
                                             meanPartySizeBank=input$ipMeanPartySizeBank,
                                             maxPartySizeBank=input$ipMaxPartySizeBank,
                                             boatShorelineBuffer= input$ipBoatShorelineBuffer,
                                             percentBank = input$ipPercentBank,
                                             mySeed=input$ipSeed)
                    )
                    
                    print(myAnglers)
                    
                    print(ggplot() +
                       geom_sf(data=myValues$lake, fill="lightskyblue") +
                       geom_sf(data=myAnglers, color="red", size=2) +
                       #geom_sf(data=myFish, color="black", size=1.5) +
                       labs(title="Anglers"))
                    
                    showNotification("Simulating Casts", duration=10, closeButton=FALSE)

                                        myCasts<-casts_place(lakeGeom=myValues$lake,
                                         myAnglers, 
                                         numberCastsPerAngler=20,
                                         meanCastDistance=input$ipCastDistanceMean,
                                         sdCastDistance=input$ipCastDistanceSd,
                                         mySeed=input$ipSeed)

                    elapsedTime=toc()
                    showNotification(paste("Simulations Complete (Elapsed Time = ", round((elapsedTime$toc-elapsedTime$tic)/60,2), " Minutes)", sep=""), duration = NULL)
                    
                    print("Done")
                    
                    tmpFish<-st_intersects(st_buffer(myFish, input$ipDetectionDistance), myCasts)
                    
                    myResults<-list()
                    tblInteractionTable<-table(tmpFish %>% lengths) %>% data.frame() %>% rename("NumberInteractions"="Var1")
                    myResults$tblInteractionTable<-tblInteractionTable
                    myResults$pltInteractionTable<-ggplot(data=myResults$tblInteractions) +
                        geom_bar(aes(x=NumberInteractions, y=Freq), stat="identity", size=2)
                    
                    
                    return(myResults)
                })
    
    output$TableInteractions=renderTable({SimsResult()$tblInteractionTable})
    output$PlotInteractions=renderPlot({ggplot(data=SimsResult()$tblInteractionTable) +
                                           geom_bar(aes(x=as.numeric(NumberInteractions), y=Freq, fill=NumberInteractions), 
                                                    stat="identity", size=2) +
                                            scale_fill_viridis_d(direction=-1) +
            scale_y_continuous(limits=c(0,40)) +
            scale_x_continuous(limits=c(0,30))+
                                            theme_bw() + 
                                            theme(legend.position="none")}) 
}

# Run the application 
shinyApp(ui = ui, server = server)
