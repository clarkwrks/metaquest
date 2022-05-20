# infoInput ---------------------------------------------------------------

infoInput_ui <- function(id, label, type, choices, info=NA, ...) {
  input_id <- NS(id, "Input")
  x <- switch(type,
              textIn = textInput(input_id, label),
              selectIn = selectInput(input_id, label, choices),
              dateIn = dateInput(input_id, label),
              textareaIn = textAreaInput(input_id, label, width = "30em", height = "8em", resize = "both")
  )
  if(!is.na(info)) {
    x %>% 
      shinyInput_label_embed(actionLink(NS(id, "Info"), icon("info-circle")))
  } else {
    x
  }
  
}

infoInput_server <- function(id, info=NA, formData = formData, ...){
  
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns
    # print(input())
    # print(id)
    # print(ns(id))
    # print(input$Input)
    # observeEvent(input, {print(input)})
    # ns_id <- paste0(id, "-Input")
    # observeEvent(input[ns_id], {
    #   print(paste0(id, "-Input"))
    #   
    # })
    observeEvent(input$Input, {
      print(input$Input)
      # paste0(id, "-Input") %>% print
      # ns("Input") %>% print
      # formInput(input$Input)
      formData[[ns("Input")]] <- input$Input
      # formInput[["test2"]] <- input$Input
      # print(formInput)
      # print(formInput %>% reactiveValuesToList())
    })
    
    if(!is.na(info)) {
      infoModal <- function(content){
        modalDialog(
          content,
          easyClose = TRUE,
          footer = tagList(
            modalButton("OK")
          )
        )
      }
      observeEvent(input$Info, {
        showModal(infoModal(info))
      })
    }
    
  })
}

infoInput_demo <- function() {
  ui <- fluidPage(
    infoInput_ui(id = "testText", label = "testText", info = "info", type = "textIn"),
    actionButton("testButton", "testButton")
    )
  server <- function(input, output, session) {
    formData <- reactiveValues("test" = "test")
    infoInput_server(id = "testText", info = "info", formData = formData)
    observeEvent(formData, {
      # print(names(formData))
      # print(formData %>% reactiveValuesToList())
    })
    observeEvent(input$testButton, {
      # print(names(formData))
      # print(formData %>% reactiveValuesToList())
    })
  }
  shinyApp(ui, server)
}

# authorForm ------------------------------------------------------------

contrib_row <- tibble::tribble(
            ~id,    ~type,        ~label,
         "name", "textIn",        "Name",
  "institution", "textIn", "Institution",
        "email", "textIn",       "Email"
  )

formListRow_ui <- function(id, ...){
  ns <- NS(id)
  # nameIn <- textInput(ns("Name"), "Name")
  # instIn <- textInput(ns("Institution"), "Institution")
  # emailIn <- textInput(ns("Email"), "Email")
  contrib_row_ns <- contrib_row %>% mutate(id = ns(id))
  
  deleteButton <- actionButton(ns("DeleteRow"), "Delete", icon("trash"))
  # print(contrib_row_ns)
  div(class = "inline formGroup", 
      # id = id,
      id = ns("div"),
      contrib_row_ns %>% pmap(infoInput_ui), deleteButton)
}

# something still off with NS, end up with formList-formList-test 
formListRow_server <- function(id, formData=formData, ...){
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns
    # cat(ns("DeleteContrib"))
    # cat(input$DeleteContrib())
    # print(contrib_row)
    # print(id)
    # print(ns(id))
    # contrib_row_ns <- contrib_row %>% mutate(id = ns(id))
    contrib_row_ns <- contrib_row
    # contrib_row_ns <- contrib_row %>% mutate(id = paste0(id))
    # print(contrib_row_ns)
    contrib_row_ns %>% pmap(infoInput_server, formData=formData)
    
    observeEvent(input$DeleteRow, {
      # insertSelector = paste0("#", id)
      insertSelector = paste0("#", ns("div"))
      # 
      # cat(input[[id]])
      
      removeUI(
        selector = insertSelector#, session = session
      )
  })
  })
}

formList_ui <- function(id,  label, ...){
  ns <- NS(id)
  
  # nameIn <- textInput(ns("Name"), "Name")
  # instIn <- textInput(ns("Institution"), "Institution")
  # emailIn <- textInput(ns("Email"), "Email")
  # nameIn <- infoInput_ui(ns("name"), "textIn", "Name")
  # instIn <- infoInput_ui(ns("institution"), "textIn", "Institution")
  # emailIn <- infoInput_ui(ns("email"), "textIn", "Email")
  
  addRow <- actionButton(ns("addRow"), "Add Row", icon("plus"), class = "fillWidth")
  div(class = "margin-panel", bs_panel(heading = label, 
           body = div(class = "y-overflow-scroll",
             # div(class = "inline formGroup",
             #     nameIn, instIn, emailIn),
             addRow))
  )
}

formList_server <- function(id, formData=formData, ...){
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns
    # 
    # infoInput_server("name", "textIn", "Name", formData)
    # infoInput_server("institution", "textIn", "Institution", formData)
    # infoInput_server("email", "textIn", "Email", formData)
    # 
    insertSelector = paste0("#", ns("addRow"))
    # nsid <- ns("test")
    counter <- reactiveVal(0)
    
    observeEvent(input$addRow, {
      counter(counter() + 1)
      # cat(ns(counter()))
      # print(id)
      
      insertUI(
        selector = insertSelector,
        where = "beforeBegin",
        ui = formListRow_ui(ns(counter()))
      )
      # formListRow_server(ns(counter()))
      formListRow_server(counter(), formData)
    })
  })
  
}

formList_demo <- function() {
  ui <- fluidPage(
    formListRow_ui("formListRowTest"),
    formList_ui("formListTest", "formListTest"))
  server <- function(input, output, session) {
    formListRow_server("formListRowTest")
    formList_server("formListTest")
  }
  shinyApp(ui, server)
}


