
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

contributor_section <- contribList_ui("contribList", "Project Contributors")
# )

general_panel <- div(preparer_section, project_section, contributor_section)

## data panel --------------------------------------------------------------


data_panel <- div()

## sensitive panel ---------------------------------------------------------


sensitive_panel <- div()


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
    top = "8em",
    height = "50%",
    right = "10px",
    width = "15%",
    bs_panel(
      heading = "Manage File",
      panel_type = "info",
      body = div(
        downloadButton(
          "exportMetaQuest",
          label = "Export MetaQuest File",
          class = "fillWidth",
          icon = shiny::icon("download")
        ),
        fileInput(
          "uploadMetaQuest",
          label = NULL,
          multiple = FALSE,
          accept = ".json",
          width = NULL,
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        actionButton("viewMetaQuest", "View",
                     class = "fillWidth",
                     icon = shiny::icon("edit")),
        actionButton("importMetaQuest", "Import",
                     class = "fillWidth",
                     icon = shiny::icon("file-import"))
  )),
  bs_panel(
    heading = "Dev",
    panel_type = "info",
    body = div(
      actionButton("showInputButton", "Show Input", class ="fillWidth"),
      actionButton("testButton", "Test", class ="fillWidth"),
      numericInput("testNumeric", "Numeric", value = 1),
      radioButtons("testToggle", NULL, list("false", "true"), "false", inline = TRUE, width = "100%")
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


# build question hooks ----------------------------------------------------
  formData <- reactiveValues("test" = "test")
  
  metaquests %>% pmap(infoInput_server, formData=formData)
  
  contribList_server("contribList")
  observeEvent(input$testButton, {
    formData %>% reactiveValuesToList %>% print
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
    inputjson <- reactiveValuesToList(input) %>% reactjson(sortKeys = TRUE)# %>% jsonlite::toJSON()
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
  

# scratch -----------------------------------------------------------------
  
  contribRows <- reactive({
    x <- reactiveValuesToList(input) 
    x_all_rows <- x %>% names %>% str_subset(., "contribList-\\d+-DeleteContrib")
    x1 <- x[x_all_rows]
    x2 <- x1 %>% keep(~ .x == 0)
    x2 %>% names %>% str_extract("contribList-\\d+")
  })
  
# 
#   observeEvent(input$testButton, {
#     # # reactiveValuesToList(input) %>% str_extract(., ))
#     # x <- reactiveValuesToList(input) 
#     # x_all_rows <- x %>% names %>% str_subset(., "contribList-\\d+-DeleteContrib")
#     # # x_all_rows %>% print
#     # # x %>% pluck(!!!x_all_rows) %>% print
#     # # x %>% map(x_all_rows) %>% print
#     # x1 <- x[x_all_rows]
#     # # x1 %>% print
#     # x2 <- x1 %>% keep(~ .x == 0)
#     contribRows() %>% print
#     current_rows <- length(contribRows()) + 1
#     import_rows <- input$testNumeric
#     if(current_rows < import_rows) {
#       add_rows <- import_rows - current_rows
#       print(add_rows)
#       for(i in 1:add_rows) {
#         delay(6000, shinyjs::click("contribList-addContrib"))
#       }
#     }
#     # while(current_rows < import_rows) {
#     #     shinyjs::click("contribList-addContrib")
#     # }
#   })
  # rowDif <- reactive({
  #   current_rows <- length(contribRows()) + 1
  #   import_rows <- input$testNumeric
  #   import_rows - current_rows
  # })
  
  
  rowDif <- reactiveVal(0)
  
  observeEvent(input$testNumeric, {
    current_rows <- isolate(contribRows()) %>% length + 1
      import_rows <- input$testNumeric
    rowDif(import_rows - current_rows)
  })
  
  observeEvent(rowDif(), {
    req(input$testToggle == "true")
    # shinyjs::click("contribList-addContrib")
    # invalidateLater(2500)
    # Sys.sleep(1)
    if(isolate(rowDif()) > 0){
      rowDif(rowDif() - 1)
      shinyjs::click("contribList-addContrib")
      print(rowDif())
    }
  })
  
  # testObserve <- observeEvent(rowDif(), {
  #   req(input$testToggle == "true")
  #   # shinyjs::click("contribList-addContrib")
  #   # 
  #   # invalidateLater(2500)
  #   # Sys.sleep(1)
  #   if(isolate(rowDif()) > 0){
  #     # shinyjs::click("contribList-addContrib")
  #     print(rowDif())
  #   }
  # })
# 
#   testObserve <- observe({
#     req(input$testToggle == "true")
#     if(isolate(rowDif()) > 0){
#       shinyjs::click("contribList-addContrib")
#       print(rowDif())
#       invalidateLater(2500)
#     }
#     # 
#     # current_rows <- length(contribRows()) + 1
#     # import_rows <- input$testNumeric
#     # # if(current_rows < import_rows){
#     # #   delay(1000, shinyjs::click("contribList-addContrib"))
#     # # }    
#     # contribRows()
#     # 
#     # if(current_rows < import_rows){
#     #   shinyjs::click("contribList-addContrib")
#     #   contribRows()
#     # }
#     # 
#     # print(xincrementer())
#     # Sys.sleep(5)
#     # xincrementer(xincrementer() + 1)
#     
#     # debounce invalidateLater isolate reactivePoll throttle
#   })
#   
  
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
