
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
library(pagedown) # render to pdf on localhost with chrome
library(shinybusy)
library(promises)
library(future)

source("utils.R")
source("fields.R")
source("stitch.R")

metaquest_fields <- read_json("metaquest_fields.json")
metaquest_version <- "1.0.0"

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
      shinyWidgets::dropdownButton(
        mgmt_panel,
        icon = icon("save"),
        right = TRUE,
        inline = TRUE,
        circle = FALSE,
        label = "File"
      ),
      shinyWidgets::dropdownButton(
        help_panel,
        icon = icon("circle-question"),
        right = TRUE,
        inline = TRUE,
        circle = FALSE,
        label = "Help"
      )
  )


# header ------------------------------------------------------------------


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
  uiOutput("saveTimerPanel")
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
            br(),
            actionLink("showRVs", "", icon("wrench"))#,
            # actionLink("plusMinutes", "", icon("plus")),
            # actionLink("mismatchInput", "", icon("table"))
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
          p("To resume working on a project, or switch to a different project, import the `.pdf` file you've previously saved to your local computer."),
          tags$ul(
            tags$li("Open the `File` menu from the top right."),
            tags$li("Click `Load`."),
            tags$li("Navigate to the file you have saved to your local computer and select it."),
            tags$li("Click `Confirm`. Fields should now automatically fill with your previous responses.")
          )
          ),
        h4("4- Submit"),
        div(p("Once you've completed all applicable fields, you will need to submit your saved/exported file to the ", 
              a("ResNet Data Portal", href = "https://data.nsercresnet.ca/",target = '_blank'), "."),
            tags$ul(
              tags$li("Follow the ", a("Data Portal User Guide", href = "https://docs.nsercresnet.ca/data-portal-guide/",target = '_blank'), " for step by step instructions.")
            )
            ),
        hr(),
        h3("Report Issues"),
        div(p("If you experience problems and/or bugs, please provide a detailed description to the developers so that they can replicate, troubleshoot, and resolve the issue."),
            p("Important information to provide includes:",
              tags$ul(
                tags$li("Date and time of incident"),
                tags$li("Description of the issue"),
                tags$li("Preceding actions"),
                tags$li("Your computer's operating system and web browser"),
                tags$li("The pdf export file you're working on"),
                tags$li("Screenshot(s) of the issue")
                )
              ),
            p("Please email issue reports to resnet.data.portal@gmail.com")
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
  observeEvent(input$showTutorialModalTimer, {
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
    reactiveValuesToList(formData)
  })
  
  output$form_peek <- renderReactjson({
    formjson() %>% reactjson(sortKeys = TRUE)
  })
  
  




# save timer ------------------------------------------------------------------



saveTime <- reactiveVal()
saveTime(now())

observeEvent(input$downloadJSON, {
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
  
  unsavedTime <- difftime(now(), saveTime(), units="mins") %>% 
    round() %>% 
    format()
  # unsavedTime <- unsavedTime %>% round() %>% format()
  
  invalidateLater(10000, session)
  
  guideLink <- actionLink("showTutorialModalTimer", 
               label= "User Guide.")
  
  if(unsavedTime > minutes(8)){
  div(style="position:fixed;width:100%;left:0;right:0;margin-top:5px",
      div(class="alert alert-danger text-center", 
          style = "min-width:10vw;max-width:30vw;margin-left:auto;margin-right:auto;padding:0;", 
          p(style="font-weight:bold;",
            paste0("Last saved: ", unsavedTime, ".")
            ),
          p(style="font-size:x-small;",
            "Unsaved work will be lost. No data is stored in this app. Please read the ", 
            guideLink)
          )
      )}
})

#  json io ----------------------------------------------------------------

## export ------------------------------------------------------------------

# export_file_name <- reactive({
#   prep_name <- (formData$`prep_name-Input`) %>% str_remove_all("[^[:alnum:]]")
#   prep_time <- as.POSIXlt(Sys.time(), tz = "UTC") %>% format("%Y-%m-%d_%H-%M-%S")
#   if(!(nchar(prep_name) > 1)) prep_name <- "unnamedPreparer"
#   paste0(prep_name, "_metaquest_", prep_time, ".json")
# })
# 
# 
# exportModal <- function(){
#   modalDialog(
#     div(
#       p("Click 'Save' to download a copy of this form to your local computer."),
#       p("Please note: your work will not be saved within this website. You can upload this file later to resume working on the form."),
#       p("You will also need to email this file to ResNet to submit your work"),
#     ),
#     title = "Save File",
#     size = "l",
#     easyClose = FALSE,
#     footer = p("Filename: ", code(export_file_name()),
#                downloadButton(
#                  "downloadJSON",
#                  label = "Save",
#                  class = "fillWidth, btn-success",
#                  icon = shiny::icon("download")
#                ),
#                modalButton("Dismiss")
#     )
#   )
# }
# 
# output$downloadJSON <- downloadHandler(
#   filename = export_file_name(),
#   content = function(file) {
#     on.exit({
#       saveTime(now())
#       removeModal()
#       })
#     formData %>% reactiveValuesToList() %>% jsonlite::write_json(., file, pretty = TRUE)
#   }
# )

export_file_name <- reactive({
  prep_name <- (formData$`prep_name-Input`) %>% str_remove_all("[^[:alnum:]]")
  prep_time <- as.POSIXlt(Sys.time(), tz = "UTC") %>% format("%Y-%m-%d_%H-%M-%S")
  if(!(nchar(prep_name) > 1)) prep_name <- "unnamedPreparer"
  paste0(prep_name, "_metaquest_", prep_time, ".pdf")
})


exportModal <- function(){
  modalDialog(
    div(
      p("Click 'Save' to download a copy of this form to your local computer."),
      p("Please note: your work will not be saved within this application. You can upload this file later to resume working on the form."),
      p("You will also need to email this file to ResNet to submit your work."),
    ),
    title = "Save File",
    size = "m",
    easyClose = FALSE,
    footer = p(
      p(style="text-align:left", "Filename: ", code(export_file_name())),
               # br(),
               downloadButton(
                 "downloadPDF",
                 label = "Save",
                 class = "fillWidth, btn-success",
                 icon = shiny::icon("download")
               ),
               modalButton("Dismiss")
    )
  )
}

output$downloadPDF <- downloadHandler(
  filename = function() {export_file_name()},
  content = function(file) {
    
        on.exit({
          saveTime(now())
          removeModal()
          })
    
    show_modal_spinner(text="Rendering report...")
    
    shiny_input <- isolate(formData %>% reactiveValuesToList())
    
    # launch the PDF file generation
    future_promise(stitchMetaquestFromShiny(
      shiny_input = shiny_input,
      metaquest_json = metaquest_fields
    ))$then(
      onFulfilled = function(value) {
        showNotification(
          paste("PDF file succesfully generated"),
          type = "message"
        )
        
        file.copy(value, file)
        
      },
      onRejected = function(error) {
        showNotification(
          error$message,
          duration = NULL,
          type = "error"
        )
        HTML("")
      }
    )$finally({
      # Sys.sleep(5)
      remove_modal_spinner
    })
  }
)


observeEvent(input$exportMetaQuest, {
  showModal(exportModal())
})


## import ------------------------------------------------------------------

importModal <- function(){
  # import_compare <- bs_collapse(id = "import_compare", content = 
  #                                 (fillRow(flex = 1, 
  #                                          bs_panel(heading = "Current File", 
  #                                                   body=reactjsonOutput("current_file_json")),
  #                                          bs_panel(heading = "Import File", 
  #                                                   body=reactjsonOutput("view_upload_json")))
  #                                 )
  # )
  modalDialog(
    # div(#style = "overflow-y:auto",
        div(
          p("Select a file previously saved from this application to resume working on it."),
          fileInput(
            "uploadMetaQuest",
            label = NULL,
            multiple = FALSE,
            accept = c(".json", ".pdf"),
            width = NULL,
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          )
          #style = "min-height:40vh;height:fit-content;max-height:60vh;overflow-y:auto",
            # bs_panel(heading = "Select a file previously saved from this application.", 
            #      body = fileInput(
            #        "uploadMetaQuest",
            #        label = NULL,
            #        multiple = FALSE,
            #        accept = c(".json", ".pdf"),
            #        width = NULL,
            #        buttonLabel = "Browse...",
            #        placeholder = "No file selected"
            #      ))#, 
        # bs_button("Show Comparison") %>% bs_attach_collapse("import_compare"), 
        # import_compare
        
    ),
    title = "Import File",
    size = "m",
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
  uploadjson()# %>% reactjson(sortKeys = TRUE)
})

uploadjson <- reactive({
  file <- input$uploadMetaQuest
  ext <- tools::file_ext(file$datapath)
  if(ext == "json") {
    print("Loading from JSON")
    uploadjson <- jsonlite::read_json(file$datapath)
  }
  if(ext == "pdf") {
    print("Loading from PDF")
    tjson <- tempfile(fileext=".json")
    print(tjson)
    system(
      str_glue("pdfdetach '{file$datapath}' -save 1 -o '{tjson}'")
    )
    uploadjson <- jsonlite::read_json(tjson)
  }
})


output$current_file_json <- renderReactjson({
  formjson() %>% reactjson(sortKeys = TRUE)
})


observeEvent(input$importConfirmButton, {
  req(input$uploadMetaQuest)
  
  file <- input$uploadMetaQuest
  # # import_data <- jsonlite::read_json(file$datapath)
  
  # # works for pdf but not json... stuck in reactive
  # import_data <- uploadjson()
  
  file <- input$uploadMetaQuest
  ext <- tools::file_ext(file$datapath)
  if(ext == "json") {
    print("Loading from JSON")
    import_data <- jsonlite::read_json(file$datapath)
  }
  if(ext == "pdf") {
    print("Loading from PDF")
    tjson <- tempfile(tmpdir = "temp", fileext=".json")
    print(tjson)
    system(
      str_glue("pdfdetach '{file$datapath}' -save 1 -o '{tjson}'")
    )
    import_data <- jsonlite::read_json(tjson)
  }
  
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
# 
# observeEvent(input$mismatchInput, {
#   # req(input$uploadMetaQuest)
#   
#   showModal(mismatchModal())
#   
# })
# 
# mismatchjson <- reactive({
#   upload_input <- uploadjson()
#   upload_input$import_time <- now()
#   form_input <- formjson()
#   mismatch_inputs <- upload_input[!names(upload_input) %in% names(form_input)]
#   mismatch_inputs %>% print
# })
# 
# 
# output$mismatch_json <- renderReactjson({
#   mismatchjson() %>% reactjson(sortKeys = TRUE)
# })
# 
# mismatchModal <- function(){
# 
#   
#   modalDialog(
#     div(#style = "min-height:60vh;overflow-y:auto",
#         textOutput("import_mismatch"),
#         verbatimTextOutput("import_mismatch_verbatim"),
#         reactjsonOutput("mismatch_json"),
#       fillRow(flex = 1,
#               bs_panel(heading = "Current File",
#                        body=reactjsonOutput("current_file_json")),
#               bs_panel(heading = "Import File",
#                        body=reactjsonOutput("view_upload_json"))
#       )
#     ),
#     title = "Mismatch",
#     size = "l",
#     footer = tagList(
#       modalButton("Close")
#     )
#   )
# }

}

# Run the application
shinyApp(ui = ui, server = server)
