#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(shinyalert)
library(sf)


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$style(".floatMe {float:left;}"),
              tags$style(".clearFloat {float:none;}"),
              tags$style(HTML("hr {text-align:left; align:left; margin-left:0; width:35%; border-top: 1px solid #000000;}"))),
    useShinyalert(),

    # Application title
    titlePanel("Geo Conversion"),

tabsetPanel(id="converts",
            type="pills",
            tabPanel(title="Shapefile To SF",
                br(),
                br(),
                div(div(shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE, class="floatMe")
                        ),
                    div(textOutput("file")
                        )
                    ),
                br(),
                br(),
                div(textInput("objectName", label="Name For R Object"),
                    span("lake_restrictions_shore"), br(),
                    span("lake_restrictions_boat"),br(),
                    span("lake_probs_shore"),br(),
                    span("lake_probs_boat")
                ),
                br(),
                br(),
                div(textInput("saveFileName", label="Name For .rData File")
                    ),
                textOutput("filepaths"),
                textOutput("paths"),
                hr(),
                div(class="clearFloat",
                    actionButton("convertShapeToSf", "Convert")
                    )
                ),
            tabPanel(title="SF to Shapefile")
            )
        
    )


# Define server logic required to draw a histogramW
server <- function(input, output, session) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
    myValues <- reactiveValues()
    
 
    roots =  c(wd = 'c:/Users/keith.hurley/Documents/GitHub/PressureSimulator/')
    
    shinyFileChoose(input, 'files', root=roots, filetypes=c('shp', 'txt'))
    #myValues$myfilepaths<-parseFilePaths(roots, input$files)$datapath[[1]]
    myValues$myfilepaths <- reactive({parseFilePaths(roots, input$files)$datapath[[1]]})
    myValues$paths<-renderText({gsub(parseFilePaths(roots, input$files)$name[[1]], "",parseFilePaths(roots, input$files)$datapath[[1]])})
    output$file<-renderText({parseFilePaths(roots, input$files)$name[[1]]})
           
     
    observeEvent(input$convertShapeToSf, {
        myValues$filepaths<-(parseFilePaths(roots, input$files)$datapath[[1]])
        assign(input$objectName, st_read(myValues$filepaths))
        save(list=input$objectName, file=paste(gsub(parseFilePaths(roots, input$files)$name[[1]], "",parseFilePaths(roots, input$files)$datapath[[1]]),
                            "/",
                            input$saveFileName,
                            ".rData",
                            sep=""))
        shinyalert("Success!", "Shapefile was converted.", type = "success",
                   closeOnClickOutside = TRUE, closeOnEsc = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
