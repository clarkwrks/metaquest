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
    
    observeEvent(input$Input, {
      formData[[ns("Input")]] <- input$Input
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

# formList ------------------------------------------------------------

contrib_row <- tibble::tribble(
            ~id,    ~type,        ~label,
         "name", "textIn",        "Name",
  "institution", "textIn", "Institution",
        "email", "textIn",       "Email"
  )

formListRow_ui <- function(id, ...){
  ns <- NS(id)
  contrib_row_ns <- contrib_row %>% mutate(id = ns(id))
  
  deleteButton <- actionButton(ns("DeleteRow"), "Delete", icon("trash"))
  # print(contrib_row_ns)
  div(class = "inline formGroup", 
      id = ns("div"),
      contrib_row_ns %>% pmap(infoInput_ui), deleteButton)
}

formListRow_server <- function(id, formData=formData, parent_id, ...){
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns

    contrib_row_ns <- contrib_row
    # print(contrib_row_ns)
    contrib_row_ns %>% pmap(infoInput_server, formData=formData)
    row_id <- id
    list_id <- id
    observeEvent(input$DeleteRow, {
      # insertSelector = paste0("#", id)
      insertSelector = paste0("#", ns("div"))

      removeUI(
        selector = insertSelector#, session = session
      )
      deleteRowFormData <- function(id, rv, ...){
        input_id <- paste0(ns(id), "-Input")
        .subset2(rv, "impl")$.values$remove(input_id)
      }
      list_nrow <- paste0(parent_id, "-Nrow")
      contrib_row_ns %>% pmap(deleteRowFormData, rv=formData)
      formData[[list_nrow]] <- formData[[list_nrow]] -1

  })
  })
}

formList_ui <- function(id,  label, ...){
  ns <- NS(id)
  
  addRow <- actionButton(ns("addRow"), "Add Row", icon("plus"), class = "fillWidth")
  div(class = "margin-panel", bs_panel(heading = label, 
           body = div(class = "y-overflow-scroll",
             addRow))
  )
}

formList_server <- function(id, formData=formData, ...){
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns

    insertSelector = paste0("#", ns("addRow"))

    counter <- reactiveVal(0)
    formData[[ns("Nrow")]] <- 0
    formListRows <- reactive({
      x <- reactiveValuesToList(formData)
      x %>% names %>% 
        str_subset(paste0(id, "-\\d+-")) %>% 
        str_extract(paste0(id, "-\\d+-")) %>%
        unique
    })

    rowDif <- reactiveVal(0)
    
    observeEvent(formData[[ns("Nrow")]], {
      ui_rows <- formListRows() %>% length
      formData_rows <- formData[[ns("Nrow")]]
      # print(paste0(ui_rows, " rows in ui, ", formData_rows, " in data"))
      rowDif(formData_rows - ui_rows)
    })
    observeEvent(input$addRow, {
      # rowDif() %>% print
      formData[[ns("Nrow")]] <- formData[[ns("Nrow")]] + 1
    })
    observeEvent(rowDif(), {
        if(rowDif() > 0){
          counter(counter() + 1)
        insertUI(
          selector = insertSelector,
          where = "beforeBegin",
          ui = formListRow_ui(ns(counter()))
        )
        formListRow_server(counter(), formData, parent_id = id)
        rowDif(rowDif() - 1)
      }
    })
    
    # observeEvent(input$addRow, {
    #   counter(counter() + 1)
    #   formData[[ns("Nrow")]] <- formData[[ns("Nrow")]] + 1
    #   insertUI(
    #     selector = insertSelector,
    #     where = "beforeBegin",
    #     ui = formListRow_ui(ns(counter()))
    #   )
    #   # formListRow_server(ns(counter()))
    #   formListRow_server(counter(), formData)
    #   formListRows() %>% print
    # })
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


