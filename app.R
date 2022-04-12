
# load libs ---------------------------------------------------------------

library(jsonlite)
library(shiny)
library(tidyverse)
library(bslib)
library(bsplus)
library(shinyjs)
library(shinythemes)
library(listviewer)
# library(shinyFeedback)
# install.packages("devtools")
# devtools::install_github('timelyportfolio/reactR')

source("quests.R")
source("utils.R")
source("mods.R")

# left area ---------------------------------------------------------------


gen_info_quests <- tribble(
  ~id, ~type, ~label, ~info, ~choices,
  "prep_name1", "textIn", "Name", "Your Name", NA,
  "no_info", "textIn", "Infotest", NA, NA
)

questions <- tribble(
  ~id, ~label, ~info, ~type, ~choices,
  "test1_id", "test1_label", "test1_info", "textIn", NA,
  "test2_id", "test2_label", "test2_info", "textIn", NA,
  "test3_id", "test3_label", "test3_info", "dateIn", NA,
  "test4_id", "test4_label", "test4_info", "selectIn", c("1", "2"),
  "test5_id", "test5_label", "test5_info", "textareaIn", NA,
  "test6_id", "test6_label", "test6_info", "textIn", NA
)

left_panel <- div(textInput_ui("testTextInput", "testing"), gen_info_quests %>% pmap(infoInput_ui))


# main area ---------------------------------------------------------------

## general panel -----------------------------------------------------------



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
          "uploadMetaQuest",
          "Upload MetaQuest File",
          multiple = FALSE,
          accept = ".json",
          width = NULL,
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        actionButton("importMetaQuest", "Import"),
        actionButton("viewMetaQuest", "View"),
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
    # useShinyFeedback(),
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
  
  textInput_server("testTextInput", "testing away")
  
  questions %>% pmap(infoInput_server)
  
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
  # 
  # observeEvent(input$prep_name, {
  #   if(nchar(input$prep_name) < 4){
  #     showFeedbackWarning(
  #       "prep_name", 
  #       text = "Please enter your name."
  #     )
  #   } else {
  #     hideFeedback("prep_name")
  #   }
  # })
  
  inputModal <- function(){
    modalDialog(
      reactjsonOutput("input_peek"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  observeEvent(input$showInputButton, {
    showModal(inputModal())
  })
  
  inputjson <- reactive({inputjson <- reactiveValuesToList(input) %>% reactjson()# %>% jsonlite::toJSON()
  })
  
  output$input_peek <- renderReactjson({
    inputjson()
      })

  # viewMetaQuest
  # importMetaQuest
  # uploadMetaquest
  
  viewMetaQuestModal <- function(){
    modalDialog(
      reactjsonOutput("view_upload_json"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  observeEvent(input$viewMetaQuest, {
    showModal(viewMetaQuestModal())
  })
  
  uploadjson <- reactive({
    file <- input$uploadMetaQuest
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "json", "Please upload a json file"))
    
    
    
    uploadjson <- read_json(file$datapath) %>% reactjson()# %>% jsonlite::toJSON()
  })
  
  output$view_upload_json <- renderReactjson({
    uploadjson()
  })
  
  session_data <- reactive({
    
  })
  
  output$exportMetaQuest <- downloadHandler(
    filename = "test.json",
    content = function(file) {
      reactiveValuesToList(input) %>% jsonlite::toJSON(., pretty = TRUE) %>% write_json(., file)}
  )
  

  
  
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
