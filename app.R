
# load libs ---------------------------------------------------------------

library(jsonlite)
library(shiny)
library(tidyverse)
library(bslib) # theme
library(bsplus) # accordion
library(shinyjs) # toggle css classes etc
library(reactR) # json viewing
library(listviewer) # also json viewing
library(shinyWidgets) # dropdown button
install.packages("remotes")
remotes::install_github("dreamRs/capture")
library(capture)

source("utils.R")
source("fields.R")

# metaquest_fields <- read_json("metaquest_fields_tester_list.json")
metaquest_fields <- read_json("metaquest_fields.json")
metaquest_version <- "0.8.5"


# main area ---------------------------------------------------------------

main_area <- buildMetaQuest_ui(metaquest_fields)

# menu --------------------------------------------------------------------


help_panel <- div(
  actionButton("showTutorialModal", 
               div("Show User Guide"),# icon("graduation-cap")), 
               width = "100%"),
  br(),
  actionButton("openResNetDocs", 
               div("ResNet Docs", tags$sup(icon("up-right-from-square"))), 
               onclick = "window.open('https://docs.nsercresnet.ca', '_blank')",
               width = "100%")
)

mgmt_panel <-    div(
  # downloadButton(
  #   "exportMetaQuest",
  #   label = "Save",
  #   class = "fillWidth",
  #   icon = shiny::icon("download")
  # ),
  actionButton(
    "exportMetaQuest",
    label = "Save",
    class = "fillWidth",
    icon = shiny::icon("download")
  ),
  actionButton(
    "importMetaQuest",
    "Load",
    class = "fillWidth",
    icon = shiny::icon("upload")
  )
)


menu_group <-
  div(
    # class="btn-group",
      shinyWidgets::dropdownButton(
        mgmt_panel,
        icon = icon("save"),
        right = TRUE,
        inline = TRUE,
        circle = FALSE,
        # margin = "0",
        # size = "lg",
        label = "File"
      ),
      shinyWidgets::dropdownButton(
        help_panel,
        icon = icon("circle-question"),
        right = TRUE,
        inline = TRUE,
        circle = FALSE,
        # margin = "0",
        # size = "lg",
        label = "Help"
      )
  )


# header ------------------------------------------------------------------

save_timer <- uiOutput("saveTimerPanel")

fixed_header <- fixedPanel(
  left = 0,
  right = 0,
  style = "background-color: white; border-bottom: solid; width:100%; z-index:9999",
  fluidRow(
    class = "fixed-header",
    column(3,
           align = "right",
           a(
             img(src = "ResNet-denser.png", height="60px"),
             href = "https://www.nsercresnet.ca/",
             target = '_blank'
           )
    ),
    column(6, align = "center",
           h2("Metadata Questionnaire", style = "text-align: center;")
    ),
    column(3,
           align = "left",
           menu_group
    )
  ),
  save_timer
  # textOutput("saveTimer")
)

# ui ----------------------------------------------------------------------

ui <- fluidPage(
  bs_theme = "flatly",
  title = "ResNet MetaQuest",
  # useShinyFeedback(),
  useShinyjs(),
  # theme = bs_theme("flatly", version = 5),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  div(fixed_header),
  
    div(id = "main-area",
        main_area, 
        div(
            align="center",
            paste0("MetaQuest v", metaquest_version),
            actionLink("showRVs", "", icon("wrench")),
            actionLink("plusMinutes", "", icon("plus"))
          ),
        capture_pdf(
          selector = "body",
          filename = "all-page",
          icon("camera"), "Take screenshot of all page",
          loading = loading()
        )
    )
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {

# build fields ----------------------------------------------------
  
  formData <- reactiveValues(version = metaquest_version)
  buildMetaQuest_server(metaquest_fields, formData)
  
# tutorial modal ----------------------------------------------------------
  tutorialModal <- function(){
    modalDialog(
      div(
        # class = "panel panel-default y-overflow-scroll scroll-shadows",
        # class = "y-overflow-scroll scroll-shadows",
        # class = "help-scroll scroll-shadows",
        # class = "help-scroll-shadows",  
      div(style = "text-align: center;", 
          em("View this guide at any time from the menu in the top right."),
          br(),
          strong("Save Early, Save Often! "),
          # em("See `Export` below.")
          " - See `Save` below.",
          hr()
          ),
      div(class="help-modal-body",
        # div(
        #   strong("Save Early, Save Often! "),
        #   # em("See `Export` below.")
        #   "See `Export` below."
        # ),
        # hr(),
        h3("How to use this form:"),
        h4("1- Fill"),
        div(
          p("Complete all relevant fields to the best of your knowledge and the information available."),
          tags$ul(
            tags$li("Click", icon("info-circle"), " for more information about a field or section."),
            tags$li("Click on section headers to expand or collapse content."),
            tags$li("Some sections may not be applicable to all projects, and include a True/False question in the header. If the answer is False, the section is not required."),
            tags$li("You may prefer to write longer passages, such as the project abstract, outside of MetaQuest and then copy/paste the content into the field.")
          ),
          ),
        h4("2- Save"),
        div(
          p("Whenever you make significant changes, save your work to your local computer. Do this frequently or you may lose your work unexpectedly!"),
          tags$ul(
            tags$li("Open the `File` menu from the top right."),
            tags$li("Click `Save`."),
            tags$li("Save the file to your local machine.")
          )
        ),
        h4("3- Load"),
        div(
          p("To resume working on a project, or switch to a different project, import the `.json` file you've previously saved to your local computer."),
          tags$ul(
            tags$li("Open the `File` menu from the top right."),
            tags$li("Click `Load`."),
            tags$li("Navigate to the file you have saved to your local computer and select it."),
            tags$li("Click `Confirm`. Fields should now automatically fill with your previous responses.")
          )
          ),
        h4("4- Submit"),
        div(p("Once you've completed all applicable fields, email your saved/export file as an attachment to the Central Team."),
            tags$ul(
              tags$li("To: resnet.data@mcgill.ca"),
              tags$li("Subject: MetaQuest - ", em("`your project title`")),
              tags$li("Attached: Exported .json file of the version you're submitting.")
            )
            ),
        hr(),
        h3("Report Issues"),
        div(p("MetaQuest is under active development and there will be problems and bugs. It's critical to provide a detailed description to the developers so that they can replicate, troubleshoot, and resolve the issue."),
            p("Important information to provide includes:",
              tags$ul(
                tags$li("Data and time of incident"),
                tags$li("Description of the issue"),
                tags$li("Preceding actions"),
                tags$li("Your computer's operating system and web browser"),
                tags$li("The .json export file you're working on"),
                tags$li("Screenshot(s) of the issue")
                )
              ),
            p("Please email issue reports to john.clark3@affiliate.mcgill.ca")
            )
      )),
      title = "MetaQuest User Guide",
      size = "l",
      easyClose = FALSE
    )
  }
  showModal(tutorialModal())
  observeEvent(input$showTutorialModal, {
    showModal(tutorialModal())
  })
  

# rv modal ----------------------------------------------------------------

  
  rvModal <- function(){
    modalDialog(
      div(style = "min-height:60vh;overflow-y:auto",
      div(class = "alert-danger", style = "text-align: center;",
          "!!!",
          div(
              "This area is for testing only. 
                      Improper use will crash the current session.",
              br(),
              "Unsaved changes will be lost."),
          "!!!"
          ),
      fillRow(flex = 1, 
              bs_panel(heading = "Shiny Input", 
                       body=reactjsonOutput("input_peek")),
              bs_panel(heading = "FormData RV", 
                       body=reactjsonOutput("form_peek")))
      ),
      size = "xl"#,
      # easyClose = TRUE
    )
  }
  
  observeEvent(input$showRVs, {
    showModal(rvModal())
  })
  
  inputjson <- reactive({
    reactiveValuesToList(input) %>% reactjson(sortKeys = TRUE)# %>% jsonlite::toJSON()
  })
  
  output$input_peek <- renderReactjson({
    inputjson()
  })
  
  
  formjson <- reactive({
    reactiveValuesToList(formData) %>% reactjson(sortKeys = TRUE)
  })
  
  output$form_peek <- renderReactjson({
    formjson()
  })
  
  


#  json io ----------------------------------------------------------------
  
## export ------------------------------------------------------------------

  export_file_name <- reactive({
      prep_name <- (formData$`prep_name-Input`) %>% str_remove_all("[^[:alnum:]]")
      prep_time <- as.POSIXlt(Sys.time(), tz = "UTC") %>% format("%Y-%m-%d_%H-%M-%S")
      if(!(nchar(prep_name) > 1)) prep_name <- "unnamedPreparer"
    paste0(prep_name, "_metaquest_", prep_time, ".json")
  })

  
  exportModal <- function(){
    modalDialog(
      div(
        p("Click 'Save' to download a copy of this form to your local computer."),
        p("Please note: your work will not be saved within this website. You can upload this file later to resume working on the form."),
        p("You will also need to email this file to ResNet to submit your work"),
          ),
      title = "Save File",
      size = "l",
      easyClose = FALSE,
      footer = p("Filename: ", code(export_file_name()),
                 downloadButton(
                   "downloadJSON",
                   label = "Save",
                   class = "fillWidth, btn-success",
                   icon = shiny::icon("download")
                 ),
                 modalButton("Dismiss")
                 )
    )
  }
  
  output$downloadJSON <- downloadHandler(
    filename = export_file_name(),
    content = function(file) {
      on.exit(removeModal())
      formData %>% reactiveValuesToList() %>% jsonlite::write_json(., file, pretty = TRUE)
    }
  )
  
  observeEvent(input$exportMetaQuest, {
    showModal(exportModal())
  })
  

## import ------------------------------------------------------------------

  importModal <- function(){
    import_compare <- bs_collapse(id = "import_compare", content = 
                                    (fillRow(flex = 1, 
                              bs_panel(heading = "Current File", 
                                       body=reactjsonOutput("current_file_json")),
                              bs_panel(heading = "Import File", 
                                       body=reactjsonOutput("view_upload_json")))
                      )
    )
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
          bs_button("Show Comparison") %>% bs_attach_collapse("import_compare"), 
          import_compare

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
    uploadjson <- jsonlite::read_json(file$datapath, simplifyVector = TRUE) %>% reactjson(sortKeys = TRUE)
  })
  
  
  output$current_file_json <- renderReactjson({
    formjson()
  })
  

observeEvent(input$importConfirmButton, {
  req(input$uploadMetaQuest)
  file <- input$uploadMetaQuest
  import_data <- jsonlite::read_json(file$datapath)
  
  valid_inputs <- import_data[str_detect(names(import_data), "-Input")]

  print("Importing")
  for(x in 1:length(valid_inputs)){
    input_name <- names(valid_inputs)[[x]]
    # print(input_name)
    # freezeReactiveValue(input, input_name)
    formData[[input_name]] <- valid_inputs[[x]]
  }
  removeModal()
})


# save timer ------------------------------------------------------------------



saveTime <- reactiveVal()

saveTime(now())

observeEvent(input$exportMetaQuest, {
  saveTime(now())
})

unsavedTime <- reactiveVal(
  now()
)

observeEvent(input$plusMinutes, {
  saveTime(saveTime() - minutes(3))
})

output$saveTimerPanel <- renderUI({
  
  req(saveTime)
  
  unsavedTime <- difftime(now(), saveTime(), units="mins")
  unsavedTime <- unsavedTime %>% round() %>% format()
  invalidateLater(50000, session)
  
  if(unsavedTime > minutes(5)){
  div(style="position:fixed;width:100%;left:0;right:0;margin-top:5px",
      div(class="alert alert-danger", style = "min-width:25vw;max-width:35vw;margin-left:auto;margin-right:auto;", 
          paste0("Last saved: ", unsavedTime, ". Unsaved work will be lost. No data is stored in this app. Please read the User Guide."))
      )}
})



}

# Run the application
shinyApp(ui = ui, server = server)
