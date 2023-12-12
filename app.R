
# load libs ---------------------------------------------------------------

library(jsonlite)
library(shiny)
library(tidyverse)
library(bslib) # theme
library(bsplus) # accordion
library(shinyjs) # toggle css classes etc
# library(reactR) # ?
library(listviewer)
# devtools::install_github('timelyportfolio/reactR')
library(shinyWidgets) # dropdown button

source("utils.R")
# source("mods.R")
# source("quests.R")
source("fields.R")

# metaquest_fields <- read_json("metaquest_fields_tester_list.json")
metaquest_fields <- read_json("metaquest_fields.json")
# metaquest_fields <- read_json("metaquest_fields_tags.json")
metaquest_version <- "0.8.0"


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
                      Improper use will crash the current session.",
                      br(),
                      "Unsaved changes will be lost."),
                  "!!!"),
    panel_type = "danger",
    body = div(
      actionButton("showInputButton", "Show Input", class ="fillWidth"),
      actionButton("showFormDataButton", "Show Form Data", class = "fillWidth")
      # hr(),
      # numericInput("testNumeric", NULL, value = 1, width = "fit-content"),
      # actionButton("testButton", "Go", class ="fillWidth")
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
                            status = FALSE, 
                            force_open = TRUE) %>%
  bs_append_noparent_toggle(title = "Developer", 
                            content = dev_panel, override_id = "devPanel", 
                            status = FALSE)



menu_row <- fluidRow(

  column(6, 
         shinyWidgets::dropdownButton(
    mgmt_panel,
    icon = icon("save"),
    right = TRUE,
    inline = TRUE,
    circle = FALSE,
    # size = "lg",
    inputId = "action_menu_test",
    label = "File"
  )
  ),
  column(6, 
         shinyWidgets::dropdownButton(
    help_panel,
    icon = icon("circle-question"),
    right = TRUE,
    inline = TRUE,
    circle = FALSE,
    # size = "lg",
    inputId = "action_menu_test2",
    label = "Help"
  )
  )
)
# 
# menu_group <-
#   div(class = "btn-group", role = "group",
#       shinyWidgets::dropdownButton(
#         mgmt_panel,
#         icon = icon("save"),
#         right = TRUE,
#         inline = TRUE,
#         circle = FALSE,
#         margin = "0",
#         # size = "lg",
#         inputId = "action_menu_test",
#         label = "File"
#       ),
#       shinyWidgets::dropdownButton(
#         help_panel,
#         icon = icon("circle-question"),
#         right = TRUE,
#         inline = TRUE,
#         circle = FALSE,
#         margin = "0",
#         # size = "lg",
#         inputId = "action_menu_test2",
#         label = "Help"
#       )#,
#       # shinyWidgets::dropdownButton(
#       #   dev_panel,
#       #   icon = icon("wrench"),
#       #   right = TRUE,
#       #   inline = TRUE,
#       #   circle = FALSE,
#       #   # size = "lg",
#       #   inputId = "action_menu_dropdown",
#       #   label = "Options"
#       # )
#       )

menu_group <-
  div(
      shinyWidgets::dropdownButton(
        mgmt_panel,
        icon = icon("save"),
        right = TRUE,
        inline = FALSE,
        circle = FALSE,
        # margin = "0",
        # size = "lg",
        inputId = "action_menu_test",
        label = "File"
      ),
      shinyWidgets::dropdownButton(
        help_panel,
        icon = icon("circle-question"),
        right = TRUE,
        inline = FALSE,
        circle = FALSE,
        # margin = "0",
        # size = "lg",
        inputId = "action_menu_test2",
        label = "Help"
      )
  )



fixed_header <- fixedPanel(
  left = 0,
  right = 0,
  style = "background-color: white; border-bottom: solid; width:100%; z-index:9999",
  fluidRow(
    class = "fixed-header",
    column(
      3,
      align = "right",
      a(
        img(src = "resnet-logo-4x-crop.png"),
        href = "https://www.nsercresnet.ca/",
        target = '_blank'
      )
    ),
    column(
      6,
      align = "center",
      h1("Metadata Questionnaire", style = "text-align: center;")
    ),
    # column(3, shinyWidgets::dropdownButton(
    #   menu_accord,
    #   icon = icon("bars"),
    #   right = TRUE,
    #   inline = TRUE,
    #   circle = FALSE,
    #   size = "lg",
    #   inputId = "action_menu_dropdown",
    #   label = "Menu"
    # ))
    column(3,
           align = "left",
           menu_group
          )
  )
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
  div(id = "main-area", main_area),
  div(style=("position:absolute; left:0; padding-left:5px;"), paste0("MetaQuest v", metaquest_version)#, 
      # shinyWidgets::dropdownButton(
      #   dev_panel,
      #   icon = icon("wrench"),
      #   right = TRUE,
      #   up=TRUE,
      #   inline = TRUE,
      #   circle = FALSE,
      #   # size = "lg",
      #   inputId = "action_menu_dropdown"
      #   # label = "Options"
      # )
      ),
  div(style=("position:absolute; right:0; padding-right:5px;"),# paste0("MetaQuest v", metaquest_version), 
      shinyWidgets::dropdownButton(
        dev_panel,
        icon = icon("wrench"),
        right = TRUE,
        up=TRUE,
        inline = TRUE,
        circle = FALSE,
        # size = "lg",
        inputId = "action_menu_dropdown"
        # label = "Options"
      )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {

# build question hooks ----------------------------------------------------
  
  ## formData rvs -----------------------------------------------------------
# 
#   formData <- reactiveValues(version = "0.0.1")
#   
#   metaquests  %>% 
#     # filter(section != "preparer") %>% 
#     pmap(infoInput_server, formData=formData)
# 
#   section_server <- function(fields, formData){
#     fields %>% pmap(infoInput_server, formData=formData)
#   }
# 
#   # preparer_section_fields %>% section_server(formData)
#   
# 
# 
#   formList_server("contribList", formData=formData, rowFields = proj_contrib_row)
#   formList_server("dataSourceList", formData=formData, rowFields = proj_contrib_row)
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
          " - See `Export` below.",
          hr()
          ),
      div(class="help-modal-body",
        # div(
        #   strong("Save Early, Save Often! "),
        #   # em("See `Export` below.")
        #   "See `Export` below."
        # ),
        # hr(),
        h3("Workflow"),
        h4("- Fill"),
        div(
          p("Complete all relevant fields to the best of your knowledge and the information available."),
          tags$ul(
            tags$li("Click", icon("info-circle"), " for more information about a field or section."),
            tags$li("Click on section headers to expand or collapse content."),
            tags$li("Some sections may not be applicable to all projects, and include a True/False question in the header. If the answer is False, the section is not required."),
            tags$li("You may prefer to write longer passages, such as the project abstract, outside of MetaQuest and then copy/paste the content into the field.")
          ),
          ),
        h4("- Export"),
        div(
          p("Whenever you make significant changes, save your work to your local computer. Do this frequently or you may lose your work unexpectedly!"),
          tags$ul(
            tags$li("Open the `Menu` from the top right."),
            tags$li("Expand `Manage File`."),
            tags$li("Click `Export`."),
            tags$li("Save the file to your local machine.")
          )
        ),
        h4("- Import"),
        div(
          p("To resume working on a project, or switch to a different project, import the `.json` file you've previously exported."),
          tags$ul(
            tags$li("Open the `Menu` from the top right."),
            tags$li("Expand `Manage File`."),
            tags$li("Click `Import`."),
            tags$li("Navigate to the file you have saved to your local computer and select it."),
            tags$li("Click `Confirm`. Fields should now automatically fill with your previous responses.")
          )
          ),
        h4("- Submit"),
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
  
# input env peek ----------------------------------------------------------

  inputModal <- function(){
    modalDialog(
      title = "Shiny Input",
      reactjsonOutput("input_peek"),
      footer = tagList(
        modalButton("Close")
      ),
      size = "l",
      easyClose = TRUE
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
      title = "formData",
      reactjsonOutput("form_peek"),
      footer = tagList(
        modalButton("Close")
      ),
      size = "l",
      easyClose = TRUE
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

  export_file_name <- reactive({
      prep_name <- (formData$`prep_name-Input`) %>% str_remove_all("[^[:alnum:]]")
      prep_time <- as.POSIXlt(Sys.time(), tz = "UTC") %>% format("%Y-%m-%d_%H-%M-%S")
      if(!(nchar(prep_name) > 1)) prep_name <- "unnamedPreparer"
    paste0(prep_name, "_metaquest_", prep_time, ".json")
  })
  output$exportMetaQuest <- downloadHandler(
    filename = export_file_name(),
    content = function(file) {
      formData %>% reactiveValuesToList() %>% jsonlite::write_json(., file, pretty = TRUE)
    }
  )

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
          bs_panel(panel_type = "danger", body = 
                     div(
                       h4(style="color:red;",
                          "Known Issue:", br(),
              "If there are multiple rows for any fields (ie you clicked 
              'Add Row' to enter data for an additonal item) you will need 
              to import twice.", br(), 
              tags$ol(tags$li("Click 'Confirm'."), 
                      tags$li("Reopen this dialog."),
                      tags$li("Click 'Confirm' once more."),
                      tags$li("Your imported data should now display correctly.")
                      ), 
                      "This is a known issue with work in progress."
              ))),
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
    # uploadjson <- jsonlite::read_json(file$datapath) %>% unlist %>% reactjson(sortKeys = TRUE)
    uploadjson <- jsonlite::read_json(file$datapath) %>% reactjson(sortKeys = TRUE)
  })
  
  
  output$current_file_json <- renderReactjson({
    formjson()
  })
  

observeEvent(input$importConfirmButton, {
  req(input$uploadMetaQuest)
  file <- input$uploadMetaQuest
  import_data <- jsonlite::read_json(file$datapath) #%>% unlist
  
  valid_inputs <- import_data[str_detect(names(import_data), "-Input")]
  # print("valid inputs")
  # print(valid_inputs)
  print("Importing")
  for(x in 1:length(valid_inputs)){
    input_name <- names(valid_inputs)[[x]]
    # print(input_name)
    # freezeReactiveValue(input, input_name)
    formData[[input_name]] <- valid_inputs[[x]]
  }
  # for(x in 1:length(valid_inputs)){
  #   input_name <- names(valid_inputs)[[x]]
  #   formData[[input_name]] <- valid_inputs[[x]]
  # }
  removeModal()
})
  
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
