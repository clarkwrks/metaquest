
# load libs ---------------------------------------------------------------

library(shiny)
library(tidyverse)
library(bslib)
library(bsplus)
library(shinyjs)
library(shinythemes)
library(listviewer)
library(shinyFeedback)
# install.packages("devtools")
# devtools::install_github('timelyportfolio/reactR')
source("utils.R")

# left area ---------------------------------------------------------------


# 
# modal_equation <-
#   bs_modal(
#     id = "modal_equation",
#     title = "Equations",
#     body = includeMarkdown(system.file("markdown", "modal.md", package = "bsplus")),
#     size = "medium"
#   )
# input_equation <-
#   selectInput(
#     inputId = "equation",
#     label = "Label with modal help",
#     choices = c("F = ma", "E = mc^2")
#   ) %>%
#   shinyInput_label_embed(
#     shiny_iconlink() %>%
#       bs_attach_modal(id_modal = "modal_equation")
#   )
#     
# test_modal <- bs_modal(id = "testmodal", title = "About this thing", body = "I am this thing")
# 
# test_panel <- bs_panel(heading = "test area", body = input_equation)

# left_panel <- div(test_panel)
left_panel <- div("")

# main area ---------------------------------------------------------------


## general panel -----------------------------------------------------------

# prep_name_input <- textInput("prep_name", "Name") %>% shinyInput_label_embed(actionLink("testInfo", icon("info-circle")))
prep_name_input <- textInputInfo("prep_name", "Name")
prep_name_info <- "Name of person preparing this form"

prep_affiliation_input <- selectInputInfo("prep_affiliation", 
                                          "Affiliation", 
                                          c("-", "Landscape 1", "Landscape 2", "Landscape 3",
                                                                               "Landscape 4", "Landscape 5", "Lanscape 6",
                                                                               "Theme 1", "Theme 2", "Theme 3", "Synthesis", "Other"))

prep_date_input <- dateInputInfo("prep_date", "Date")

prep_email_input <- textInputInfo("prep_email", "Email")

prep_title_input <- textInputInfo("proj_title", "Project Title")

proj_abstract_input <- textAreaInputInfo("proj_abstract", "Project Abstract", width = "30em", height = "8em", resize = "both")

preparer_section <- bs_panel(heading = "About the metadata preparer",
                             body = div(class = "inline formGroup",
                                        prep_name_input,
                                        prep_affiliation_input,
                                        prep_email_input,
                               prep_date_input,
                               prep_title_input))
project_section <- bs_panel(heading = "About the research project",
                                      body = div(class = "inline extra-wide",
                                                 proj_abstract_input
                             ))
                             
general_panel <- div(preparer_section, project_section)

## data panel --------------------------------------------------------------


data_desc_section <- div(
  bs_panel(heading = "Location", body =
             ""),
  bs_panel(heading = "Data Considerations", body =
           div(class = "inline",
             checkboxInput("showTestPanel", "Research product contains sensitive data", TRUE),
             checkboxInput("validTestPanel", "Toggle test panel status", FALSE),
             radioButtons("dataConsiderationRadios", "Data Considerations", c("True" = TRUE, "False" = FALSE), selected = character(0), inline = TRUE)
           ))
)

data_panel <- div(data_desc_section)

## sensitive panel ---------------------------------------------------------

sensitive_section <- bs_panel(heading = "Sensitivity", 
                            body = div(
                              checkboxInput("validCheck", "Am I valid?", FALSE),
                            )
                              )

sensitive_panel <- div(sensitive_section)


## sources panel -----------------------------------------------------------

sources_panel <- div("")

## spatial panel -----------------------------------------------------------

spatial_panel <- div("")


## build accordion --------------------------------------------------------

main_panel <- bs_accordion(id = "mainPanelAccord") %>% 
  bs_set_opts(panel_type = "primary", use_heading_link = FALSE
              ) %>%
  bs_append_noparent_toggle(title = "General Information", 
            content = general_panel, override_id = "generalPanel") %>%
  bs_append_noparent_toggle(title = "Data Description", 
            content = data_panel, override_id = "dataPanel") %>%
  bs_append_noparent_toggle(title = "Sensitive Data", 
            content = sensitive_panel, override_id = "sensitivePanel",
            condition = "Involves sensitive data?") %>%
  bs_append_noparent_toggle(title = "Data Sources", 
            content = sources_panel, override_id = "sourcePanel",
            condition = "Incorporates external data?") %>%
  bs_append_noparent_toggle(title = "Spatial Data", 
            content = spatial_panel, override_id = "spatialPanel",
            condition = "Contains spatial data?")


# right area --------------------------------------------------------------

right_panel <-
  fixedPanel(
    top = "63px",
    height = "50%",
    right = "10px",
    width = "15%",
    bs_panel(
      heading = "Manage File",
      panel_type = "info",
      body = div(
        fileInput(
          "importMetaQuest",
          "Import MetaQuest File",
          multiple = FALSE,
          accept = ".csv",
          width = NULL,
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        downloadButton(
          "exportMetaQuest",
          label = "Export MetaQuest File",
          class = "fillWidth",
          icon = shiny::icon("download")
        ),
        verbatimTextOutput("lastModified"),
        radioButtons("testToggle", "Test", 
                     c("True" = TRUE, "False" = FALSE), selected = character(0), inline = TRUE),
        actionButton("showInputButton", "Show Input")
        )
  )
  )




# ui ----------------------------------------------------------------------


ui <- fluidPage(
    bs_theme = "flatly",
    useShinyjs(),
  # theme = bs_theme("flatly", version = 5),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    # titlePanel(fluidRow(
    #   column(3, img(src = "resnet-logo-4x.png")), 
    #   column(9, h1("Metadata Questionnaire", align = "center"))), 
    #   "Metadata Questionnaire"),
    titlePanel(h1("Metadata Questionnaire", align = "center"), 
      "Metadata Questionnaire"),
    fillRow(flex = c(1,4,1),
            left_panel,
            main_panel,
            right_panel,
            id = "mainTab")
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {

  output$lastModified <- renderPrint({ "foo" })
  
  # observeEvent(input$showInputButton, {
  #   showModal(modalDialog(
  #     title = "Input",
  #     "test"
  #   ))
  # })
  # observe({
  #   toggle("sensitivePanel", input$dataConsiderationRadios)
  # })
  # 
  # observe({
  #   toggleClass("sensitivePanel", "panel-danger", !input$validCheck)
  # })
  # 
  

  
  inputModal <- function(){
    modalDialog(
      reactjsonOutput("input_peek"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  inputjson <- reactive({inputjson <- reactiveValuesToList(input) %>% reactjson()# %>% jsonlite::toJSON()
  })
  
  observeEvent(input$showInputButton, {
    showModal(inputModal())
    
  })
  
  infoModal <- function(infoText) {
    modalDialog(
      infoText,
      easyClose = TRUE,
      footer = tagList(
        modalButton("OK")
        )
    )
  }
  observeEvent(input$prep_nameInfo, {
    showModal(infoModal(prep_name_info))
    
  })
  
  
  output$input_peek <- renderReactjson({
    inputjson()
      })


  
  
  observe({
    x <- input$sourcePanelToggle
    toggleClass("sourcePanel", "panel-info", is.null(x))
    toggleClass("sourcePanel", "panel-primary", isTRUE(as.logical(x)))
    toggleClass("sourcePanel", "panel-default", isFALSE(as.logical(x)))
  })
  
  observe({
    x <- input$sensitivePanelToggle
    toggleClass("sensitivePanel", "panel-info", is.null(x))
    toggleClass("sensitivePanel", "panel-primary", isTRUE(as.logical(x)))
    toggleClass("sensitivePanel", "panel-default", isFALSE(as.logical(x)))
  })
  
  observe({
    x <- input$spatialPanelToggle
    toggleClass("spatialPanel", "panel-info", is.null(x))
    toggleClass("spatialPanel", "panel-primary", isTRUE(as.logical(x)))
    toggleClass("spatialPanel", "panel-default", isFALSE(as.logical(x)))
  })
  # session$onSessionEnded(function() {
  #   isolate(saveRDS( input, file = 'integer.RDS'))
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
