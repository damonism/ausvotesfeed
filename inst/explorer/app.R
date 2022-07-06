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

tmp_xml_dir <- rappdirs::user_data_dir("ausvotesfeed")
if(!dir.exists(tmp_xml_dir)) {
  dir.create(tmp_xml_dir)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2022 Federal Election"),

    tabsetPanel(type = "tabs",
                tabPanel("Data",
                         p("Data current as of ", textOutput("xml_updated", inline = TRUE)),
                         actionButton("fetch", "Update media feed data"),
                         checkboxInput("api", "Use API", value = FALSE),
                         conditionalPanel(
                           condition = "input.api == true",
                           textInput("host", "API Host")
                         )
                ),
                tabPanel("TCP Leading",
                         checkboxInput("tcp_changed", "Only show seats that have changed hands (incumbent trailing)"),
                         dataTableOutput("tcp_lead")),
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

    # results_xml <- reactive({

      tmp_files_list <- list.files(tmp_xml_dir, pattern = "aec-mediafeed-.*\\.zip", full.names = TRUE)
      if(length(tmp_files_list) > 0){
        tmp_filename <- rev(tmp_files_list)[1]
        message("Using existing media feed file: ", tmp_filename)
        read_mediafeed_xml(tmp_filename)
        v$data <- read_mediafeed_xml(tmp_filename)
        # v$updated <- as.POSIXct(get_mediafeed_metadata(v$data)["Created"], format = "%FT%T")
      }
    # })
    # v$data <- read_mediafeed_xml(download_mediafeed_file(2022, Filetype = "Verbose", Archive = FALSE))

    observeEvent(input$fetch, {

      if(input$api == TRUE) {

        tmp_xml <- download_mediafeed_api(input$host, 2022, Filetype = "Verbose", Archive = FALSE)

      } else {

        tmp_xml <- download_mediafeed_file(2022, Filetype = "Verbose", Archive = FALSE)

      }

      tmp_filename <- rev(strsplit(tmp_xml, "/")[[1]])[1]
      tmp_new_xml_file <- paste0(tmp_xml_dir, "/", tmp_filename)
      # file.copy(tmp_xml, tmp_new_xml_file)
      if(!file.copy(tmp_xml, tmp_new_xml_file, overwrite = TRUE)) {
        stop("Failed to copy file to ", tmp_new_xml_file)
      }

      v$data <- read_mediafeed_xml(tmp_new_xml_file)

      results_xml <- reactive({read_mediafeed_xml(tmp_new_xml_file)})

      # v$updated <- as.POSIXct(get_mediafeed_metadata(v$data)["Created"], format = "%FT%T")
      showModal(modalDialog("Results updated."))
    })

    output$xml_updated <- renderText({
      as.character(as.POSIXct(get_mediafeed_metadata(v$data)["Created"], format = "%FT%T"))
      # as.character(as.POSIXct(get_mediafeed_metadata(results_xml())["Created"], format = "%FT%T"))
    })

    # All of the display data as a reactive object
    display_data <- reactive({
        mediafeed_display(v$data, mf_cand)
    })

    #### UI functions ####

    output$div_select <- renderUI({
        div_df <- display_data()$Divisions[order(display_data()$Divisions$DivisionNm),]
        div_labels <- div_df$DivisionID
        names(div_labels) <- div_df$DivisionNm
        selectInput("division", label = "Select division", choices = div_labels)
    })

    #### TCP Functions ####

    tcp_data <- reactive({
      tmp_tcp <- get_mediafeed_votes_div(v$data, "tcp")
      tmp_tcp <- merge(tmp_tcp, mf_cand, by = c("CandidateID", "DivisionID", "CandidateType"))

      # Previous party
      tmp_inc <- mf_cand[mf_cand$Incumbent == TRUE | mf_cand$IncumbentNotional == TRUE,][c("DivisionID", "PartyNm", "PartyCode")]
      colnames(tmp_inc) <- c("DivisionID", "PartyNm.Prev", "PartyCode.Prev")
      merge(tmp_tcp, tmp_inc, by = c("DivisionID"), all.x = TRUE)
    })

    output$tcp_lead <- renderDataTable({
      tmp_tcp_data <- tcp_data()
      tmp_tcp_data$PartyCode <- ifelse(is.na(tmp_tcp_data$PartyCode), "IND", tmp_tcp_data$PartyCode)
      tmp_tcp_data$PartyCode.Prev <- ifelse(is.na(tmp_tcp_data$PartyCode.Prev), "IND", tmp_tcp_data$PartyCode.Prev)
      tmp_tcp_data$PartyNm <- ifelse(tmp_tcp_data$IsIndependent == TRUE, "Independent", tmp_tcp_data$PartyNm)

      if(input$tcp_changed) {
        tmp_tcp_data <- tmp_tcp_data[tmp_tcp_data$PartyCode != tmp_tcp_data$PartyCode.Prev,]
      }
      tmp_tcp_data[tmp_tcp_data$TCP.Percentage > 50,][c("DivisionNm", "StateAb", "CandidateNm", "Gender", "PartyCode.Prev", "PartyCode", "PartyNm", "TCP.Votes", "TCP.Percentage")]
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
      tmp_div_chart <- div_data()$fp
      tmp_div_chart <- tmp_div_chart[tmp_div_chart$CandidateType == "Candidate",]

      tmp_data <- rbind(data.frame(PartyNm = tmp_div_chart$PartyNm, Percent = tmp_div_chart$FP.Percentage, Type = "FP"),
                        data.frame(PartyNm = tmp_div_chart$PartyNm, Percent = tmp_div_chart$FP.Swing, Type = "Swing"))

      ggplot(tmp_data, aes(y = PartyNm, x = Percent, fill = Type)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal()
    })

    output$div_tcp <- renderTable({
        div_data()$tcp[c("CandidateNm", "PartyNm", "CandidateType", "TCP.Votes", "TCP.Margin", "TCP.Percentage", "TCP.Swing", "Status")]
    },
    hover = TRUE, na = "")

}

# Run the application
shinyApp(ui = ui, server = server)
