
# load libs ---------------------------------------------------------------

# library(jsonlite)
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

left_area <- div() 

# main area ---------------------------------------------------------------

## general panel -----------------------------------------------------------

# project_section <- bs_panel(heading = "About the research project",
#                                       body = div(class = "inline extra-wide",
#                                                  proj_abstract_input
#                              ))
#                              
# general_panel <- div(preparer_section, project_section)

preparer_section <- bs_panel(heading = "About the metadata preparer", 
                           body = div(class = "inline formGroup",
                                      metaquests %>% 
                                        filter(panel == "general" & section == "preparer") %>%
                                        pmap(infoInput_ui)))

project_section <- bs_panel(heading = "About the research project", 
                          body = div(class = "inline formGroup",
                                     metaquests %>% 
                                       filter(panel == "general" & section == "project") %>%
                                       pmap(infoInput_ui),
                                     contribList_ui("contribList", "Project Contributors"))
                          )

# contributor_section <- bs_panel(heading = "Project Contributors", 
#                             body = div(class = "inline formGroup",
#                                        contribList_ui("contribList"))
# )

general_panel <- div(preparer_section, project_section, contribRow_ui("testrow"))

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

main_area <- bs_accordion(id = "mainPanelAccord") %>% 
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

right_area <-
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
            left_area,
            main_area,
            right_area,
            id = "mainTab")
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {

  # output$lastModified <- renderPrint({input$`prep_name-Input`})
  

# build question hooks ----------------------------------------------------
  contribRow_server("testrow")
  metaquests %>% pmap(infoInput_server)
  
  contribList_server("contribList")
  
  # infoModal <- function(infoText) {
  #   modalDialog(
  #     infoText,
  #     easyClose = TRUE,
  #     footer = tagList(
  #       modalButton("OK")
  #     )
  #   )
  # }

# input env peek ----------------------------------------------------------

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
  
  inputjson <- reactive({
    inputjson <- reactiveValuesToList(input) %>% reactjson()# %>% jsonlite::toJSON()
  })
  
  output$input_peek <- renderReactjson({
    inputjson()
  })

  # viewMetaQuest
  # importMetaQuest
  # uploadMetaquest

#  json io ----------------------------------------------------------------
  


## export ------------------------------------------------------------------


  current_data <- reactive({
      list(
        prep_name = input$`prep_name-Input`,
        prep_affiliation = input$`prep_affiliation-Input`,
        prep_email = input$`prep_email-Input`,
        prep_date = input$`prep_date-Input`,
        proj_title = input$`proj_title-Input`,
        proj_abstract = input$`proj_abstract-Input`
      )
  })
  
  output$exportMetaQuest <- downloadHandler(
    filename = "test.json",
    content = function(file) {
      # reactiveValuesToList(input) %>% jsonlite::toJSON(., pretty = TRUE) %>% write_json(., file)
      current_data() %>% jsonlite::write_json(., file, pretty = TRUE)
    }
  )

## import ------------------------------------------------------------------

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
    # validate(need(ext == "json", "Please upload a json file"))
    
    
    
    uploadjson <- jsonlite::read_json(file$datapath) %>% reactjson()# %>% jsonlite::toJSON()
  })
  
  output$view_upload_json <- renderReactjson({
    uploadjson()
  })
  
  observeEvent(input$importMetaQuest, {
    file <- input$uploadMetaQuest
    import_json <- jsonlite::read_json(file$datapath)
    updateTextInput(session, "prep_name-Input", value = import_json$prep_name)
    updateSelectInput(session, "prep_affiliation-Input", selected = import_json$prep_affiliation)
    updateTextInput(session, "prep_email-Input", value = import_json$prep_email)
    updateDateInput(session, "prep_date-Input", value = import_json$prep_date %>% unlist)
    updateTextInput(session, "proj_title-Input", value = import_json$proj_title)
    updateTextAreaInput(session, "proj_abstract-Input", value = import_json$proj_abstract)
  })
# 
#   prep_name = input$`prep_name-Input`,
#   prep_affiliation = input$`prep_affiliation-Input`,
#   prep_email = input$`prep_email-Input`,
#   prep_date = input$`prep_date-Input`,
#   proj_title = input$`proj_title-Input`,
#   proj_abstract = input$`proj_abstract-Input`
#   
# panel modals ------------------------------------------------------------

  
  
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
