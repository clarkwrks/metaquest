
# load libs ---------------------------------------------------------------

# library(jsonlite)
library(shiny)
library(tidyverse)
library(bslib)
library(bsplus)
library(shinyjs)
library(reactR)
library(listviewer)
# devtools::install_github('timelyportfolio/reactR')
library(shinyWidgets)

source("utils.R")
source("mods.R")
source("quests.R")
source("fields.R")

# main area ---------------------------------------------------------------

## general panel -----------------------------------------------------------


preparer_section <- bs_panel(heading = " About the metadata preparer", class="panel-section",
                            body = div(class = "inline form-group",
                                       metaquests %>% 
                                         filter(panel == "general" & section == "preparer") %>%
                                         pmap(infoInput_ui)
                                       )
                            )

project_section <- bs_panel(heading = "About the research project", class="panel-section",
                          body = div(class = "inline form-group",
                                     metaquests %>% 
                                       filter(panel == "general" & section == "project") %>%
                                       pmap(infoInput_ui),
                                     div(selectizeInput("selectizeTest", label = "Keywords", choices = c("test"),
                                                        multiple = TRUE, options =  list(create = TRUE))
                                         )
                                     )
                          )
contributor_section <- bs_panel(heading = "Project Contributors", class="panel-section",
                                body = formList_ui("contribList"))

general_panel <- div(preparer_section, project_section, contributor_section)

## data panel --------------------------------------------------------------

data_panel <- div()

## exceptions panel ---------------------------------------------------------

exceptions_panel <- bs_panel(heading = "", class="panel-section",
                            body = exception_section)

## sources panel -----------------------------------------------------------

data_sources_panel <- formList_ui("dataSourceList", "Data Sources", rowFields = proj_contrib_row)

sources_panel <- div(data_sources_panel)
# sources_panel <- div()

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
  bs_append_noparent_toggle(title = "Data Exceptions", 
            content = exceptions_panel, override_id = "exceptionsPanel",
            condition = "Restrictions on publication?") %>%
  bs_append_noparent_toggle(title = "Data Sources", 
            content = sources_panel, override_id = "sourcePanel",
            condition = "Incorporates external data?") %>%
  bs_append_noparent_toggle(title = "Spatial Data", 
            content = spatial_panel, override_id = "spatialPanel",
            condition = "Contains spatial data?")



# menu --------------------------------------------------------------------


help_panel <- div(
  actionButton("showTutorialModal", 
               div("Show Tutorial"),# icon("graduation-cap")), 
               width = "100%"),
  br(),
  actionButton("openResNetDocs", 
               div("ResNet Docs", tags$sup(icon("external-link-alt fa-xs"))), 
               onclick = "window.open('https://docs.nsercresnet.ca', '_blank')",
               width = "100%")
)

mgmt_panel <-    div(
  downloadButton(
    "exportMetaQuest",
    label = "Export",
    class = "fillWidth",
    icon = shiny::icon("download")
  ),
  actionButton(
    "importMetaQuest",
    "Import",
    class = "fillWidth",
    icon = shiny::icon("upload")
  )
)

## dev panel ----

dev_panel <-  
  bs_panel(
    heading = div(class = "alert-danger", style = "text-align: center;",
                  "!!!",
                  div(style="font-size:x-small", 
                      "This area is for testing only. 
                      Improper use will crash the current session. 
                      Unsaved changes will be lost."),
                  "!!!"),
    panel_type = "danger",
    body = div(
      actionButton("showInputButton", "Show Input", class ="fillWidth"),
      actionButton("showFormDataButton", "Show Form Data", class = "fillWidth"),
      hr(),
      numericInput("testNumeric", NULL, value = 1, width = "12em"),
      actionButton("testButton", "Go", class ="fillWidth")
      
    )
    )

menu_accord <- bs_accordion(id = "menuAccord") %>% 
  bs_set_opts(panel_type = "default", use_heading_link = FALSE
  ) %>%
  bs_append_noparent_toggle(title = "Help", 
                            content = help_panel, override_id = "helpPanel", 
                            status = FALSE) %>%
  bs_append_noparent_toggle(title = "Manage File", 
                            content = mgmt_panel, override_id = "mgmtPanel", 
                            status = FALSE) %>%
  bs_append_noparent_toggle(title = "Developer", 
                            content = dev_panel, override_id = "devPanel", 
                            status = FALSE)


fixed_menu <- fixedPanel(left = 0, right = 0,
                         style = "background-color: honeydew; border-bottom: solid; width:100%; z-index:9999",
                         fluidRow(class = "topmenu",
                                  column(3, align="right", img(src = "resnet-logo-4x.png")),
                                  column(6, align="center", h1("Metadata Questionnaire", style = "text-align: center;")),
                                  column(3, align="left", shinyWidgets::dropdownButton(
                                    menu_accord,
                                    icon = icon("gear"),
                                    right = TRUE,
                                    inline = TRUE,
                                    circle = FALSE,
                                    size = "lg",
                                    inputId = "action_menu_dropdown",
                                    label = "Menu"
                                  ))))

# ui ----------------------------------------------------------------------


ui <- fluidPage(
  bs_theme = "flatly",
  useShinyjs(),
  title = "ResNet MetaQuest",
  # useShinyFeedback(),
  # theme = bs_theme("flatly", version = 5),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  # titlePanel(fluidRow(
  #   column(3, img(src = "resnet-logo-4x.png")), 
  #   column(9, h1("Metadata Questionnaire", align = "center"))), 
  #   "Metadata Questionnaire"),
  # titlePanel(h1("Metadata Questionnaire", align = "center"), 
  #   "Metadata Questionnaire"),
  div(fixed_menu),
  div(style = "padding-top:8em;", main_area)
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {

# build question hooks ----------------------------------------------------
  
  ## formData rvs -----------------------------------------------------------

  formData <- reactiveValues(version = "0.0.1")
  
  metaquests  %>% 
    # filter(section != "preparer") %>% 
    pmap(infoInput_server, formData=formData)

  section_server <- function(fields, formData){
    fields %>% pmap(infoInput_server, formData=formData)
  }

  # preparer_section_fields %>% section_server(formData)
  


  formList_server("contribList", formData=formData, rowFields = proj_contrib_row)
  formList_server("dataSourceList", formData=formData, rowFields = proj_contrib_row)

# tutorial modal ----------------------------------------------------------
  tutorialModal <- function(){
    modalDialog(
      div(
        tags$ul(
          tags$li("Save early"), 
          tags$li("Save often"), 
          tags$li("Avoid special characters"),
          tags$li("Click", icon("info-circle"), " for more info")
        )
      ),
      title = "Getting Started with MetaQuest",
      size = "xl"
    )
  }
  
  observeEvent(input$showTutorialModal, {
    showModal(tutorialModal())
  })
  
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
    reactiveValuesToList(input) %>% reactjson(sortKeys = TRUE)# %>% jsonlite::toJSON()
  })
  
  output$input_peek <- renderReactjson({
    inputjson()
  })

# form data peek ----------------------------------------------------------

  formDataModal <- function(){
    modalDialog(
      reactjsonOutput("form_peek"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  observeEvent(input$showFormDataButton, {
    showModal(formDataModal())
  })
  
  formjson <- reactive({
    reactiveValuesToList(formData) %>% reactjson(sortKeys = TRUE)
  })
  
  output$form_peek <- renderReactjson({
    formjson()
  })

#  json io ----------------------------------------------------------------
  
## export ------------------------------------------------------------------

  output$exportMetaQuest <- downloadHandler(
    filename = "test.json",
    content = function(file) {
      formData %>% reactiveValuesToList() %>% jsonlite::write_json(., file, pretty = TRUE)
    }
  )

## import ------------------------------------------------------------------

  importModal <- function(){
    modalDialog(
      div(style = "min-height:60vh;overflow-y:auto",
          bs_panel(heading = "Select File to Import", 
                   body = fileInput(
            "uploadMetaQuest",
            label = NULL,
            multiple = FALSE,
            accept = ".json",
            width = NULL,
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          )),
      fillRow(flex = 1, 
              bs_panel(heading = "Current File", 
                       body=reactjsonOutput("current_file_json")),
              bs_panel(heading = "Import File", 
                       body=reactjsonOutput("view_upload_json"))
              )
      ),
      title = "Import File",
      size = "l",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("importConfirmButton", "Confirm")
    )
    )
  }
  observeEvent(input$importMetaQuest, {
    showModal(importModal())
  })
  
  output$view_upload_json <- renderReactjson({
    uploadjson()
  })
  
  uploadjson <- reactive({
    file <- input$uploadMetaQuest
    ext <- tools::file_ext(file$datapath)
    req(file)
    # validate(need(ext == "json", "Please upload a json file"))
    uploadjson <- jsonlite::read_json(file$datapath) %>% unlist %>% reactjson(sortKeys = TRUE)# %>% jsonlite::toJSON()
  })
  
  
  output$current_file_json <- renderReactjson({
    formjson()
  })
  

observeEvent(input$importConfirmButton, {
  file <- input$uploadMetaQuest
  import_data <- jsonlite::read_json(file$datapath) %>% unlist
  
  valid_inputs <- import_data[str_detect(names(import_data), "-Input")]
  # print("valid inputs")
  # print(valid_inputs)
  for(x in 1:length(valid_inputs)){
    input_name <- names(valid_inputs)[[x]]
    formData[[input_name]] <- valid_inputs[[x]]
  }
  removeModal()
})
  
# panel modals ------------------------------------------------------------

  observe({
    x <- input$sourcePanelToggle
    toggleClass("sourcePanel", "panel-info", is.null(x))
    toggleClass("sourcePanel", "panel-primary", isTRUE(as.logical(x)))
    toggleClass("sourcePanel", "panel-default", isFALSE(as.logical(x)))
  })
  
  observe({
    x <- input$exceptionsPanelToggle
    toggleClass("exceptionsPanel", "panel-info", is.null(x))
    toggleClass("exceptionsPanel", "panel-primary", isTRUE(as.logical(x)))
    toggleClass("exceptionsPanel", "panel-default", isFALSE(as.logical(x)))
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
  

# scratch -----------------------------------------------------------------
  # 
  # contribRows <- reactive({
  #   x <- reactiveValuesToList(input) 
  #   x_all_rows <- x %>% names %>% str_subset(., "contribList-\\d+-DeleteContrib")
  #   x1 <- x[x_all_rows]
  #   x2 <- x1 %>% keep(~ .x == 0)
  #   x2 %>% names %>% str_extract("contribList-\\d+")
  # })
  # 
  # rowDif <- reactiveVal(0)
  # 
  # observeEvent(input$testNumeric, {
  #   current_rows <- isolate(contribRows()) %>% length + 1
  #     import_rows <- input$testNumeric
  #   rowDif(import_rows - current_rows)
  # })
  # 
  # observeEvent(rowDif(), {
  #   req(input$testToggle == "true")
  #   # shinyjs::click("contribList-addContrib")
  #   # invalidateLater(2500)
  #   # Sys.sleep(1)
  #   if(isolate(rowDif()) > 0){
  #     rowDif(rowDif() - 1)
  #     shinyjs::click("contribList-addRow")
  #     print(rowDif())
  #   }
  # })
  
  # https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/
  # https://github.com/rstudio/shiny/issues/2374
  # https://stackoverflow.com/questions/51515641/delete-corresponding-input-element-when-using-removeui
  # https://stackoverflow.com/questions/60259473/shiny-reactive-input-add-and-delete
  # https://gist.github.com/zappingseb/440691f109192be43eee239c5b2cee76
  # https://community.rstudio.com/t/nested-server-modules-how-to-use-global-instead-of-local-namespace-inside-inner-module/103815/5
  # https://stackoverflow.com/questions/63060605/r-shiny-insertui-and-observeevent-in-module
  # https://github.com/rstudio/shiny/issues/2439
  # https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
}

# Run the application
shinyApp(ui = ui, server = server)
