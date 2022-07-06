
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

source("quests.R")
source("utils.R")
source("mods.R")

# left area ---------------------------------------------------------------

left_area <- div() 

# main area ---------------------------------------------------------------

## general panel -----------------------------------------------------------

preparer_section <- bs_panel(heading = "About the metadata preparer", 
                           body = div(class = "inline formGroup",
                                      metaquests %>% 
                                        filter(panel == "general" & section == "preparer") %>%
                                        pmap(infoInput_ui)))

project_section <- bs_panel(heading = "About the research project", 
                          body = div(class = "inline formGroup",
                                     metaquests %>% 
                                       filter(panel == "general" & section == "project") %>%
                                       pmap(infoInput_ui)
                                     )
                          )

contributor_section <- formList_ui("contribList", "Project Contributors")
# )

general_panel <- div(preparer_section, project_section, contributor_section)

## data panel --------------------------------------------------------------

data_panel <- div()

## sensitive panel ---------------------------------------------------------

sensitive_panel <- div()

## sources panel -----------------------------------------------------------

data_sources_panel <- formList_ui("dataSourceList", "Data Sources")

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

## mgmt panel ----
# 
# mgmt_panel <-    bs_panel(
#       heading = "Manage File",
#       panel_type = "info",
#       body = div(
#         downloadButton(
#           "exportMetaQuest",
#           label = "Export MetaQuest File",
#           class = "fillWidth",
#           icon = shiny::icon("download")
#         ),
#         fileInput(
#           "uploadMetaQuest",
#           label = NULL,
#           multiple = FALSE,
#           accept = ".json",
#           width = NULL,
#           buttonLabel = "Browse...",
#           placeholder = "No file selected"
#         ),
#         actionButton("viewMetaQuest", "View",
#                      class = "fillWidth",
#                      icon = shiny::icon("edit")),
#         actionButton("importMetaQuest", "Import",
#                      class = "fillWidth",
#                      icon = shiny::icon("file-import"))
#   ))

mgmt_panel <-    div(
    downloadButton(
      "exportMetaQuest",
      label = "Export",
      class = "fillWidth",
      icon = shiny::icon("download")
    ),
    actionButton("importMetaQuest", "Import",
                 class = "fillWidth",
                 icon = shiny::icon("upload"))
)

## dev panel ----

dev_panel <-  
  bs_panel(
    heading = div(class = "alert-danger", style = "text-align: center;",
                  "!!!", br(), 
                  div(style="font-size:x-small", "This area is for testing only.", br(),
                      "Improper use will crash the current session. Unsaved changes will be lost."),
                  "!!!", br()),
    panel_type = "danger",
    body = div(
      actionButton("showInputButton", "Show Input", class ="fillWidth"),
      actionButton("showFormDataButton", "Show Form Data", class = "fillWidth"),
      actionButton("testButton", "Test", class ="fillWidth"),
      numericInput("testNumeric", "Numeric", value = 1),
      radioButtons("testToggle", NULL, list("false", "true"), "false", inline = TRUE, width = "100%")
    )
    )

right_area_accord <- bs_accordion(id = "rightPanelAccord") %>% 
  bs_set_opts(panel_type = "default", use_heading_link = FALSE
  ) %>%
  bs_append_noparent_toggle(title = "Manage File", 
                            content = mgmt_panel, override_id = "mgmtPanel", 
                            status = FALSE) %>%
  bs_append_noparent_toggle(title = "Developer", 
                            content = dev_panel, override_id = "devPanel", 
                            status = FALSE)

right_area <-
  fixedPanel(
    top = "8em",
    height = "50%",
    right = "10px",
    width = "15%",
    right_area_accord)
    
# ui ----------------------------------------------------------------------

fixed_menu <- fixedPanel(left = 0, right = 0,
  style = "background-color: white; border-bottom: solid; width:100%; z-index:9999",
  fluidRow(
column(3, img(src = "resnet-logo-4x.png"), style ="padding-left: 1em"),
column(6, h1("Metadata Questionnaire", align = "center")),
column(3, shinyWidgets::dropdownButton(
  div(right_area_accord),
  icon = icon("gear"),
  right = TRUE,
  inline = FALSE,
  circle = FALSE,
  size = "lg",
  inputId = "action_menu_dropdown",
  label = "Menu"
))))

# 
# fixed_menu <- fixedPanel(left = 0, right = 0,
#                          style = "background-color: white; border-bottom: solid; width:100%; z-index:9999", 
#                          fluidRow(
#                            column(2, img(src = "resnet-logo-4x.png"), style ="padding-left: 1em"),
#                            column(8, h1("Metadata Questionnaire", align = "center")),
#                            column(2, shinyWidgets::dropMenu(
#                              actionButton(
#                                inputId = "drop",
#                                label = "Menu",
#                                icon = icon("gear"),
#                                class = "btn-success",
#                                placement = "bottom-start"
#                              ),
#                              div(right_area_accord)
#                            ))))

  
ui <- fluidPage(
  bs_theme = "flatly",
  useShinyjs(),
  title = "MetaQuest",
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
  div(style = "padding-top:8em;",main_area)
)

# ui <- fluidPage(
#     bs_theme = "flatly",
#     useShinyjs(),
#     # useShinyFeedback(),
#   # theme = bs_theme("flatly", version = 5),
#     tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
#     # titlePanel(fluidRow(
#     #   column(3, img(src = "resnet-logo-4x.png")), 
#     #   column(9, h1("Metadata Questionnaire", align = "center"))), 
#     #   "Metadata Questionnaire"),
#     # titlePanel(h1("Metadata Questionnaire", align = "center"), 
#     #   "Metadata Questionnaire"),
#     fillRow(
#       flex = c(5,1),
#       # flex = c(1,4,1),
#       # left_area,
#       div(fixedRow(
#           column(1, img(src = "resnet-logo-4x.png")),
#           column(10, h1("Metadata Questionnaire", align = "center")),
#           column(1, shinyWidgets::dropdownButton(
#             right_area_accord,
#             icon = icon("gear"),
#             right = TRUE,
#             circle = FALSE,
#             size = "lg"
#           )
#           )), 
#           main_area),
#       right_area,
#       id = "mainTab")
# )

# server ------------------------------------------------------------------

server <- function(input, output, session) {


# build question hooks ----------------------------------------------------
  
  ## formData rvs -----------------------------------------------------------

  formData <- reactiveValues(version = "0.0.1")
  
  metaquests %>% pmap(infoInput_server, formData=formData)
  
  formList_server("contribList", formData=formData)
  formList_server("dataSourceList", formData=formData)
  # observeEvent(input$testButton, {
  #   formData %>% reactiveValuesToList %>% print
  # })
  
  observeEvent(input$testButton, {
    formData[["contribList-Nrow"]] <- input$testNumeric
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
    reactiveValuesToList(formData) %>% reactjson(sortKeys = TRUE)# %>% jsonlite::toJSON()
  })
  
  output$form_peek <- renderReactjson({
    formjson()
  })
 

#  json io ----------------------------------------------------------------
  
## export ------------------------------------------------------------------

  output$exportMetaQuest <- downloadHandler(
    filename = "test.json",
    content = function(file) {
      # reactiveValuesToList(input) %>% jsonlite::toJSON(., pretty = TRUE) %>% write_json(., file)
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
  

### old_import --------------------------------------------------------------

  
  # observeEvent(input$importConfirmButton, {
  #   file <- input$uploadMetaQuest
  #   import_data <- jsonlite::read_json(file$datapath) %>% unlist
  #   
  #   # 
  #   # # maybe pass list inputs as nested list?
  #   # # or setup module_server observer for list items in `formData` but not `input`
  #   # # delete all listForm items on import, then create rows as needed?
  #   # 
  #   form_data <- reactiveValuesToList(formData)
  #   
  #   import_data_names <- names(import_data)
  #   list_inputs <- import_data[str_detect(import_data_names, "-Nrow")]
  #   for(x in 1:length(list_inputs)){
  #     list_nrow <- as.numeric(list_inputs[[x]])
  #     list_nrow_name <- names(list_inputs)[x]
  #     list_name <- list_nrow_name %>% str_remove("-Nrow")
  #     if(list_nrow > 0){
  #       if(formData[[list_nrow_name]] < list_nrow){
  #         formData[[list_nrow_name]] <- list_nrow
  #       }
  #       
  #       # # recapture formData to see if updated
  #       form_data <- reactiveValuesToList(formData)
  # 
  #       import_list_rows <- names(import_data) %>%
  #         str_extract(str_c(list_name, "-\\d+-")) %>%
  #         na.omit %>% unique %>% sort
  #       form_list_rows <- names(form_data) %>%
  #         str_extract(str_c(list_name, "-\\d+-")) %>%
  #         na.omit %>% unique %>% sort %>%
  #         c(import_list_rows) %>% unique# combine
  # 
  #       # import_list_rows <- c(form_list_rows, import_list_rows) %>% unique %>% .[1:list_nrow]
  #       # print(import_list_rows)
  #       # 
  #       # # loop through list inputs, extract all rows, rename to existing row numbers
  #       for(x in 1:length(import_list_rows)){
  #         import_data_names <- str_replace(import_data_names, import_list_rows[[x]], form_list_rows[[x]])
  #       }
  #       # print(import_data_names)
  #       # # still have a reactive chain race... setting formData["contribList-Nrow] <- x works outside of this observer,
  #       # # but here contribList-Nrow already = the import, because module server sees all inputs exist.
  #       # # Either prioritize the row creation in the module, somehow, before the formData is changed,
  #       # # or rework the listForm module observers.
  #       # # Extract to function?
  #       # # Isolate something on the module side? Kills me that the test button increment works but not here
  #     }
  #   }
  # 
  #   mod_data <- set_names(import_data, import_data_names)
  #     valid_inputs <- mod_data[str_detect(names(mod_data), "-Input")]
  #     for(x in 1:length(valid_inputs)){
  #       input_name <- names(valid_inputs)[[x]]
  #       formData[[input_name]] <- valid_inputs[[x]]
  #       }
  # 
  #   # # old, working version
  #   # import_json <- jsonlite::read_json(file$datapath) %>% unlist
  #   # import_json <- import_data
  #   # valid_inputs <- import_json[str_detect(names(import_json), "-Input")]
  #   # for(x in 1:length(valid_inputs)){
  #   #   input_name <- names(valid_inputs)[[x]]
  #   #   formData[[input_name]] <- valid_inputs[[x]]
  #   # }
  #   # 
  #   # 
  #   removeModal()
  # })

### new new import ---------------------------------------  
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

### new import --------------------------------------------------------------
# 
# 
#   mod_data_for_import <- reactiveVal()
#   
#   observeEvent(input$importConfirmButton, {
#     file <- input$uploadMetaQuest
#     import_data <- jsonlite::read_json(file$datapath) %>% unlist
#     
#     # 
#     # # maybe pass list inputs as nested list?
#     # # or setup module_server observer for list items in `formData` but not `input`
#     # # delete all listForm items on import, then create rows as needed?
#     # 
#     form_data <- reactiveValuesToList(formData)
#     
#     import_data_names <- names(import_data)
#     list_inputs <- import_data[str_detect(import_data_names, "-Nrow")]
#     for(x in 1:length(list_inputs)){
#       list_nrow <- as.numeric(list_inputs[[x]])
#       list_nrow_name <- names(list_inputs)[x]
#       list_name <- list_nrow_name %>% str_remove("-Nrow")
#       if(list_nrow > 0){
#         if(formData[[list_nrow_name]] < list_nrow){
#           formData[[list_nrow_name]] <- list_nrow
#         }
#         
#         # # recapture formData to see if updated
#         # form_data <- reactiveValuesToList(formData)
#         # 
#         # import_list_rows <- names(import_data) %>%
#         #   str_extract(str_c(list_name, "-\\d+-")) %>%
#         #   na.omit %>% unique %>% sort
#         # form_list_rows <- names(form_data) %>%
#         #   str_extract(str_c(list_name, "-\\d+-")) %>%
#         #   na.omit %>% unique %>% sort %>%
#         #   c(import_list_rows) %>% unique# combine
#         # import_list_rows <- c(form_list_rows, import_list_rows) %>% unique %>% .[1:list_nrow]
#         # 
#         # # loop through list inputs, extract all rows, rename to existing row numbers
#         # for(x in 1:length(import_list_rows)){
#           # import_data_names <- str_replace(import_data_names, import_list_rows[[x]], form_list_rows[[x]])
#         # }
# 
#       }
#       print("current rows")
#       reactiveValuesToList(formData) %>% names %>%
#         str_extract(str_c(list_name, "-\\d+-")) %>%
#         na.omit %>% unique %>% sort %>% print
#     }
# 
#     
#     # valid_inputs <- import_data[str_detect(names(import_data), "-Input")]
#     mod_data_for_import(import_data)
#     # 
#     # mod_data <- set_names(import_data, import_data_names)
#     # for(x in 1:length(valid_inputs)){
#     #   input_name <- names(valid_inputs)[[x]]
#     #   mod_data_for_import[[input_name]] <- valid_inputs[[x]]
#     # }
#     
#     # # old, working version
#     # import_json <- jsonlite::read_json(file$datapath) %>% unlist
#     # import_json <- import_data
#     # valid_inputs <- import_json[str_detect(names(import_json), "-Input")]
#     # for(x in 1:length(valid_inputs)){
#     #   input_name <- names(valid_inputs)[[x]]
#     #   formData[[input_name]] <- valid_inputs[[x]]
#     # }
#     # 
#     # 
#     
# 
#     
#     removeModal()
#   })
#   
#   
#   observe({
#     # mod_import <- reactiveValuesToList(mod_data_for_import)
#     import_data <- mod_data_for_import()
#     req(length(import_data) > 0)
#     print(import_data)
#     isolate({
#       print("copying import")
#         form_data <- reactiveValuesToList(formData)
# 
#         import_data_names <- names(import_data)
#         list_inputs <- import_data[str_detect(import_data_names, "-Nrow")]
#         for(x in 1:length(list_inputs)){
#           list_nrow <- as.numeric(list_inputs[[x]])
#           list_nrow_name <- names(list_inputs)[x]
#           list_name <- list_nrow_name %>% str_remove("-Nrow")
#           if(list_nrow > 0){
# 
# 
#             # # recapture formData to see if updated
# 
#             import_list_rows <- names(import_data) %>%
#               str_extract(str_c(list_name, "-\\d+-")) %>%
#               na.omit %>% unique %>% sort
#             form_list_rows <- names(form_data) %>%
#               str_extract(str_c(list_name, "-\\d+-")) %>%
#               na.omit %>% unique %>% sort %>%
#               c(import_list_rows) %>% unique# combine
#             # print(import_list_rows)
#             # print("form list rows")
#             # names(form_data) %>%
#               # str_extract(str_c(list_name, "-\\d+-")) %>%
#               # na.omit %>% unique %>% sort %>% print
#             import_list_rows <- c(form_list_rows, import_list_rows) %>% unique %>% .[1:list_nrow]
#             #
#             # # loop through list inputs, extract all rows, rename to existing row numbers
#             for(x in 1:length(import_list_rows)){
#               import_data_names <- str_replace(import_data_names, import_list_rows[[x]], form_list_rows[[x]])
#             }
#             # print(import_data_names)
#             # # still have a reactive chain race... setting formData["contribList-Nrow] <- x works outside of this observer,
#             # # but here contribList-Nrow already = the import, because module server sees all inputs exist.
#             # # Either prioritize the row creation in the module, somehow, before the formData is changed,
#             # # or rework the listForm module observers.
#             # # Extract to function?
#             # # Isolate something on the module side? Kills me that the test button increment works but not here
#           }
#         }
# 
#         mod_data <- set_names(import_data, import_data_names)
#           valid_inputs <- mod_data[str_detect(names(mod_data), "-Input")]
#           # print("valid inputs")
#           # print(valid_inputs)
#           for(x in 1:length(valid_inputs)){
#             input_name <- names(valid_inputs)[[x]]
#             formData[[input_name]] <- valid_inputs[[x]]
#             }
#       })
#     })


  # 
  # viewMetaQuestModal <- function(){
  #   modalDialog(
  #     reactjsonOutput("view_upload_json"),
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("ok", "OK")
  #     )
  #   )
  # }
  # 
  # observeEvent(input$viewMetaQuest, {
  #   showModal(viewMetaQuestModal())
  # })
  # 
  # uploadjson <- reactive({
  #   file <- input$uploadMetaQuest
  #   ext <- tools::file_ext(file$datapath)
  #   
  #   req(file)
  #   # validate(need(ext == "json", "Please upload a json file"))
  #   
  #   uploadjson <- jsonlite::read_json(file$datapath) %>% reactjson()# %>% jsonlite::toJSON()
  # })
  # 
  # output$view_upload_json <- renderReactjson({
  #   uploadjson()
  # })
  # 
  # observeEvent(input$importMetaQuest, {
  #   file <- input$uploadMetaQuest
  #   import_json <- jsonlite::read_json(file$datapath)
  #   updateTextInput(session, "prep_name-Input", value = import_json$prep_name)
  #   updateSelectInput(session, "prep_affiliation-Input", selected = import_json$prep_affiliation)
  #   updateTextInput(session, "prep_email-Input", value = import_json$prep_email)
  #   updateDateInput(session, "prep_date-Input", value = import_json$prep_date %>% unlist)
  #   updateTextInput(session, "proj_title-Input", value = import_json$proj_title)
  #   updateTextAreaInput(session, "proj_abstract-Input", value = import_json$proj_abstract)
  # })
  
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
