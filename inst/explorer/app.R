#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
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
                         tableOutput("div_fp"),
                         plotOutput("div_fp_chart"),
                         # dataTableOutput("div_fp"),
                         h3("Two-candidate preferred"),
                         tableOutput("div_tcp")
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #### Data functions ####

    v <- reactiveValues(data = NULL)

    observeEvent(input$fetch, {
        # withProgress("Fetching new results", value = .5, {
            v$data <- read_mediafeed_xml(download_mediafeed_file(2022, Filetype = "Verbose", Archive = FALSE))
        # })

        v$updated <- as.POSIXct(get_mediafeed_metadata(v$data)["Created"], format = "%FT%T")
        showModal(modalDialog("Results downloaded: ", v$updated))
    })

    # All of the display data as a reactive object
    display_data <- reactive({
        mediafeed_display(v$data, mf_cand)
    })

    #### UI functions ####

    output$div_select <- renderUI({
        div_df <- display_data()$Divisions[order(display_data()$Divisions$DivisionNm),]
        div_labels <- div_df$DivisionId
        names(div_labels) <- div_df$DivisionNm
        selectInput("division", label = "Select division", choices = div_labels)
    })

    #### Division functions ####

    # The division data as a reactive object
    div_data <- reactive({
        req(input$division)
        display_data()[[as.character(input$division)]]
    })

    output$div_fp <- renderDataTable({
        display_data()[[as.character(input$division)]]$fp[c("CandidateNm", "PartyNm", "CandidateType", "FP.Votes", "FP.Percentage", "FP.Swing", "Status")]
    })

    # renderTable is quicker and neater, renderDataTable is more powerful, but looks ugly by default
    output$div_fp <- renderTable({
        div_data()$fp[c("CandidateNm", "PartyNm", "CandidateType", "FP.Votes", "FP.Percentage", "FP.Swing", "Status")]
    },
    hover = TRUE, na = "")

    output$div_fp_chart <- renderPlot({
        tmp_data <- rbind(data.frame(PartNm = div_data()$fp$PartyNm, Percent = div_data()$fp$FP.Percentage, Type = "FP"),
                          data.frame(PartNm = div_data()$fp$PartyNm, Percent = div_data()$fp$FP.Swing, Type = "Swing"))

        ggplot(tmp_data, aes(y = PartNm, x = Percent, fill = Type)) +
            geom_bar(stat = "identity", position = position_dodge())
    })

    output$div_tcp <- renderTable({
        div_data()$tcp[c("CandidateNm", "PartyNm", "CandidateType", "TCP.Votes", "TCP.Margin", "TCP.Percentage", "TCP.Swing", "Status")]
    },
    hover = TRUE, na = "")

}

# Run the application
shinyApp(ui = ui, server = server)
