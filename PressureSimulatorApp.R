options(stringsAsFactors=FALSE)


source("./SimulationScripts/Functions_prep.R")
#devtools::install_github('wleepang/shiny-directory-input')
library(shinyDirectoryInput)
library(shinybusy)

# Define UI for application that draws a histogram
ui <- fluidPage(
    use_busy_spinner(spin = "orbit"),
    
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
                                              )),
                                      tags$br(),
                                      selectInput('ipLakeGeom',
                                                  'Select A Lake',
                                                  choice = list.dirs(path='./data/lakes/',recursive=FALSE, full.names = FALSE)
                                                  ),
                                      tabsetPanel(type="tabs",
                                                  tabPanel("Restrictions: ",
                                                           tags$div(style="background: gainsboro; padding:10px",
                                                                    uiOutput("restrictionShoreSelection"),
                                                                    uiOutput("restrictionLakeSelection"),
                                                                    uiOutput("restrictionFishSelection")
                                                                    )
                                                            ),
                                                  tabPanel("Area Probabilities: ",
                                                           tags$div(style="background: gainsboro; padding:10px",
                                                                    uiOutput("probShoreSelection"),
                                                                    uiOutput("probLakeSelection"),
                                                                    uiOutput("probFishSelection")
                                                                    )
                                                           )
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
                                                       tags$div(style="background: gainsboro; padding:10px; margin-bottom:10px;",
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
                                                  value=1),
                                      selectInput("ipFishDistributionType",
                                                  "Distribution Type",
                                                  choices=c("Random", 
                                                            "Schooling"),
                                                  selected="Schooling"),
                                      conditionalPanel("input.ipFishDistributionType == 'Schooling'",
                                                       tags$span(style="font-weight:bold",
                                                                 "Schooling Fish Options: "),
                                                       tags$div(style="background: gainsboro; padding:10px; margin-bottom:10px;",
                                                                sliderInput("ipMeanNumberPerSchool",
                                                                            "Mean Number Of Fish Per School",
                                                                            min=2,
                                                                            max=1000,
                                                                            step=1,
                                                                            value=10),
                                                                sliderInput("ipSchoolingDistance",
                                                                            "Max Distance Between Two Schooling Fish (Max Radius Of School = (Mean Number Per School / 2) * Max Distance Between Two Schooling Fish)",
                                                                            min=0.25,
                                                                            max=3,
                                                                            step=0.25,
                                                                            value=1),
                                                                
                                                       )
                                      ),
                                      ),
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
                                                  value = 20),
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
                                                  step = 1,
                                                  value = 1),
                                      sliderInput("ipNumberOfCores",
                                                  "Number Of Cores:",
                                                  min=1,
                                                  max=96,
                                                  step=1,
                                                  value=2),
                                      sliderInput("ipGroupSize",
                                                  "Parallel Group Size:",
                                                  min=3,
                                                  max=25,
                                                  step=1,
                                                  value=10),
                                      checkboxInput(inputId='ipSaveOutputs', label="Save Outputs?", value=TRUE),
                                        
                                      conditionalPanel("input.ipSaveOutputs == true",
                                              tags$div(style="background: gainsboro; padding:20px; margin-left:20px; margin-bottom:20px;",
                                                       directoryInput(inputId='directory', label = 'Select a directory:', value = './outputs/'),
                                                       textInput(inputId="ipSaveName", label="Base name for saved files:", value="delete_me_dev"),
                                                       textInput(inputId="ipRunName", label="Run Name:", value="Default Run In Shiny"),
                                                       textInput(inputId="ipRunDescription", label="Description Of Run:", value="This is the default description for a run.")
                                              )
                                        ),
                                      
                                      actionButton("doSims", 
                                                   "Run Simulations")
                                      )
                             )
                         ),
                tabPanel("Results",
                         tags$div(style="max-width:700px;",
                                  tags$div(style="float:left; width:200px;",
                                           tags$span(style="font-weight: bold",
                                                     "Simulation #1 Results:"),
                                           tags$br(),
                                           tags$br(),
                                           tableOutput('TableInteractions')),
                                  tags$div(style="float:left; width:500px; padding-left:50px;",
                                           textOutput('TextElapsedTime'),
                                           textOutput('TextMeanInteractions'),
                                           tags$br(),
                                           tags$br(),
                                           plotOutput('PlotInteractions')
                                           )
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
    #myResults<-reactiveVal()
    #myResults(NULL)
    

    #this runs the save directory selection box
    observeEvent(
        ignoreNULL = TRUE,
        eventExpr = {
            input$directory
        },
        handlerExpr = {
            if (input$directory > 0) {
                # condition prevents handler execution on initial app launch
                
                # launch the directory selection dialog with initial path read from the widget
                path = choose.dir(default = readDirectoryInput(session, 'directory'))
                
                # update the widget value
                updateDirectoryInput(session, 'directory', value = path)
            }
        }
    )
    
    #setup lake name and acres display
    observeEvent(input$ipLakeGeom,
                 {
                     #load(file="./data/lakes/round_1/lake.rData")
                     load(file=paste("./data/lakes/", input$ipLakeGeom, "/lake.rData", sep=""))
                     myValues$lake=lake
                     myValues$lakeName=lake$name[1]
                     myValues$lakeAcres=as.numeric(round(st_area(lake)/4046.86,1))
                     rm(lake)
                 })
    
    output$restrictionShoreSelection <- renderUI({
        selectInput("ipShoreRestrictions", 
                    "Select A Shoreline Restriction: ",
                    choice = c("None", gsub("//.rData", "", list.files(path=paste("./data/lakes/",input$ipLakeGeom,"/restrictions/shore/", sep=""), pattern=".rData", recursive=FALSE, full.names = FALSE))))
                    })
    
    output$restrictionLakeSelection <- renderUI({
        selectInput("ipLakeRestrictions", 
                    "Select A Lake Restriction: ",
                    choice = c("None", gsub("//.rData", "", list.files(path=paste("./data/lakes/",input$ipLakeGeom,"/restrictions/lake/", sep=""), pattern=".rData", recursive=FALSE, full.names = FALSE))))
    })
    
    output$restrictionFishSelection <- renderUI({
        selectInput("ipFishRestrictions", 
                    "Select A Fish Restriction: ",
                    choice = c("None", gsub("//.rData", "", list.files(path=paste("./data/lakes/",input$ipLakeGeom,"/restrictions/fish/", sep=""), pattern=".rData", recursive=FALSE, full.names = FALSE))))
    })
    
    output$probShoreSelection <- renderUI({
        selectInput("ipShoreProbs", 
                    "Select A Shoreline Probability Map: ",
                    choice = c("None", gsub("//.rData", "", list.files(path=paste("./data/lakes/",input$ipLakeGeom,"/probs/shore/", sep=""), pattern=".rData", recursive=FALSE, full.names = FALSE))))
    })
    
    output$probLakeSelection <- renderUI({
        selectInput("ipLakeProbs", 
                    "Select A Lake Probability Map: ",
                    choice = c("None", gsub("//.rData", "", list.files(path=paste("./data/lakes/",input$ipLakeGeom,"/probs/lake/", sep=""), pattern=".rData", recursive=FALSE, full.names = FALSE))))
    })
    
    output$probFishSelection <- renderUI({
        selectInput("ipFishProbs", 
                    "Select A Fish Probability Map: ",
                    choice = c("None", gsub("//.rData", "", list.files(path=paste("./data/lakes/",input$ipLakeGeom,"/probs/fish/", sep=""), pattern=".rData", recursive=FALSE, full.names = FALSE))))
    })
    
    #must set output options or renderUI calls on hidden tabs won't set values unless tab is shown
    outputOptions(output, "probShoreSelection", suspendWhenHidden = FALSE)
    outputOptions(output, "probLakeSelection", suspendWhenHidden = FALSE) 
    outputOptions(output, "probFishSelection", suspendWhenHidden = FALSE) 
    
    
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
                     show_modal_spinner(spin="orbit", text="Running Simulations - Keep Your Arms And Legs Inside The Vehicle")
                     updateTabsetPanel(session, "PageNav",
                                  selected = "Results")
                     SimsResult()
                     remove_modal_spinner()
                 })

    #run Simulations
    SimsResult<-eventReactive(input$doSims,
                {
                    #load lake object
                    #myLakeObject<<-obj_create_default_lake_object()
                    myLakeObject<<-obj_create_lake_object(lakeGeom_path = paste("data/lakes/", input$ipLakeGeom, "/lake.rData", sep=""),
                                                          restrictionsShore_path = ifelse(
                                                              input$ipShoreRestrictions=="None",
                                                              NA,
                                                              paste("./data/lakes/", input$ipLakeGeom, "/restrictions/shore/", input$ipShoreRestrictions, sep="")
                                                              ),
                                                          restrictionsLake_path = ifelse(
                                                              input$ipLakeRestrictions=="None",
                                                              NA,
                                                              paste("./data/lakes/", input$ipLakeGeom, "/restrictions/lake/", input$ipLakeRestrictions, sep="")
                                                          ),
                                                          restrictionsFish_path = ifelse(
                                                              input$ipFishRestrictions=="None",
                                                              NA,
                                                              paste("./data/lakes/", input$ipLakeGeom, "/restrictions/fish/", input$ipFishRestrictions, sep="")
                                                          ),
                                                          probsShore_path = ifelse(
                                                              input$ipShoreProbs=="None",
                                                              NA,
                                                              paste("./data/lakes/", input$ipLakeGeom, "/probs/shore/", input$ipShoreProbs, sep="")
                                                          ),
                                                          probsLake_path = ifelse(
                                                              input$ipLakeProbs=="None",
                                                              NA,
                                                              paste("./data/lakes/", input$ipLakeGeom, "/probs/lake/", input$ipLakeProbs, sep="")
                                                          ),
                                                          probsFish_path = ifelse(
                                                              input$ipFishProbs=="None",
                                                              NA,
                                                              paste("./data/lakes/", input$ipLakeGeom, "/probs/fish/", input$ipFishProbs, sep="")
                                                          )
                    )
                    # ggplot() +
                    #     geom_sf(data=myLakeObject$lakeGeom, color="blue") +
                    #     geom_sf(data=myLakeObject$restrictionsShore, color="red")

                    #create parameter object
                    #myParamsObject<<-obj_create_default_parameters_object()
                    myParamsObject<<-obj_create_parameters_object(
                        acres=myLakeObject$lakeAcres,
                        hoursPerAcre=input$ipHoursPerAcre,
                        tripLengthMean=input$ipTripLengthMean,
                        tripLengthSd=input$ipTripLengthSd,
                        castsPerHourMean=input$ipCastsPerHourMean,
                        castsPerHourSd=input$ipCastsPerHourSd,
                        castDistanceMean=input$ipCastDistanceMean,
                        castDistanceSd=input$ipCastDistanceSd,
                        numberFish=input$ipNumberFish,
                        fishShorelineBuffer=input$ipFishShorelineBuffer,
                        fishDistribution=input$ipFishDistributionType,
                        fishMeanNumberPerSchool=input$ipMeanNumberPerSchool,
                        fishSchoolingDistance=input$ipSchoolingDistance,
                        anglerBoatDistribution=input$ipAnglerDistributionType,
                        anglerBankDistribution=input$ipAnglerDistributionType,
                        anglerBoatPartyRadius=input$ipBoatAnglerPartyClusterRadius,
                        anglerBankPartyRadius=input$ipBankAnglerPartyClusterRadius,
                        meanPartySizeBoat=input$ipMeanPartySizeBoat,
                        maxPartySizeBoat=input$ipMaxPartySizeBoat,
                        meanPartySizeBank=input$ipMeanPartySizeBank,
                        maxPartySizeBank=input$ipMaxPartySizeBank,
                        boatShorelineBuffer=input$ipBoatShorelineBuffer,
                        percentBank=input$ipPercentBank
                    )
                    

                    #create simulations object
                    # mySimsObject<<-obj_create_default_simulations_object()
                    # mySimsObject$parGroupSize=input$ipGroupSize
                    # mySimsObject$parNumberCores=input$ipNumberCores
                    mySimsObject<<-obj_create_simulations_object(
                        numberSimulations=input$ipNumberSimulations,
                        runName=input$ipRunName,
                        runDescription=input$ipRunDescription,
                        saveNamePath=input$directory,
                        saveNameBase=input$ipSaveName,
                        seed=input$ipSeed,
                        parGroupSize=input$ipGroupSize, #must be at least 3?
                        parNumberCores=input$ipNumberOfCores)
                    

                    myResults<<-sims_runSimulations(myLakeObject,
                                        myParamsObject,
                                        mySimsObject)
                    
                    myValues$tblInteractions<<-myResults$interactionCounts %>%
                        filter(simId==1) %>%
                        group_by(numInteractions) %>%
                        summarise(Freq=n()) %>%
                        rename("Number Of Interactions"=numInteractions,
                               "Frequency"=Freq)

                    
                    myValues$meanInteractionsPerFish<-round(myResults$myFish %>%
                                                                summarise(myMean=mean(InteractionsMean, na.rm=TRUE)),1)
                    
                    myValues$timings$ElapsedTime = myResults$timings$ElapsedTime
                    
                    #save output
                    #include flag if full run completed?
                    if(input$ipSaveOutputs==TRUE){

                         save_simulation_run(fileNameAndPath=paste(readDirectoryInput(session, 'directory'),
                                                                    input$ipSaveName,
                                                                     save_create_timestamp(),
                                                                     sep=""),
                                 
                                             myLakeObject,
                                             myParamsObject,
                                             mySimsObject,
                                             myResults)
                    }
                    
                    return(TRUE)
                })
    
        output$TableInteractions=renderText({})
        output$TableInteractions=renderTable({myValues$tblInteractions})
        
        output$PlotInteractions=renderPlot({ggplot(data=myValues$tblInteractions)  +
                                            geom_bar(aes(x=`Number Of Interactions`, y=Frequency, fill=`Number Of Interactions`),
                                                     stat="identity") +
                 labs(x="Number Of Interactions", y="Frequency", title="Fish/Angler Interactions") +
                                             scale_fill_viridis_c(direction=-1) +
             #scale_y_continuous(limits=c(0,max(myValues$tblInteractions$numInteractions)+20)) +
             #scale_x_continuous(limits=c(-1,max(as.numeric(as.character(myValues$tblInteractions$numInteractions)))))+
                                             theme_bw() +
                                             theme(legend.position="none")
        })
          
        output$TextMeanInteractions=renderText({paste("Mean Interactions Per Fish: ",
                                                      myValues$meanInteractionsPerFish, sep="")})
        
        output$TextElapsedTime=renderText(paste("Elapsed Time: ", 
                                                myValues$timings$ElapsedTime,
                                                " minutes", sep=""))
        
}

# Run the application 
shinyApp(ui = ui, server = server)
