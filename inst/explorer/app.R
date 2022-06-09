#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ausvotesfeed)
load("preload_2022.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2022 Federal Election"),

    tabsetPanel(type = "tabs",
                tabPanel("Data",
                         actionButton("fetch", "Fetch media feed data")
                         ),
                tabPanel("By Division",
                         uiOutput("div_select"),
                         h3("First preferences"),
                         dataTableOutput("div_fp"),
                         h3("Two-candidate preferred"),
                         dataTableOutput("div_tcp")
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    v <- reactiveValues(data = NULL)

    observeEvent(input$fetch, {
        v$data <- read_mediafeed_xml(download_mediafeed_file(2022, Filetype = "Verbose", Archive = FALSE))
        v$updated <- as.POSIXct(get_mediafeed_metadata(v$data)["Created"], format = "%FT%T")
        showModal(modalDialog("Results downloaded: ", v$updated))
    })

    display_data <- reactive({
        mediafeed_display(v$data, mf_cand)
    })

    output$div_select <- renderUI({
        div_labels <- display_data()$Divisions$DivisionId
        names(div_labels) <- display_data()$Divisions$DivisionNm
        selectInput("division", label = "Select division", choices = div_labels)
    })

    output$div_fp <- renderDataTable({
        display_data()[[as.character(input$division)]]$fp[c("CandidateNm", "PartyNm", "CandidateType", "FP.Votes", "FP.Percentage", "FP.Swing", "Status")]
    })

    output$div_tcp <- renderDataTable({
        display_data()[[as.character(input$division)]]$tcp[c("CandidateNm", "PartyNm", "CandidateType", "TCP.Votes", "TCP.Margin", "TCP.Percentage", "TCP.Swing", "Status")]
    })

}

# Run the application
shinyApp(ui = ui, server = server)
