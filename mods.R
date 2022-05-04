# infoInput ---------------------------------------------------------------

infoInput_ui <- function(id, label, type, choices, info, ...) {
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

infoInput_server <- function(id, info, ...){
  if(!is.na(info)) {
    
    moduleServer(id, function(input, output, session) {
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
    })
  }
}

# authorForm ------------------------------------------------------------


contribRow_ui <- function(id, ...){
  ns <- NS(id)
  nameIn <- textInput(ns("Name"), "Name")
  instIn <- textInput(ns("Institution"), "Institution")
  emailIn <- textInput(ns("Email"), "Email")
  deleteButton <- actionButton(ns("DeleteContrib"), "Delete", icon("trash"))
  div(class = "inline formGroup", 
      # id = id,
      id = ns("div"),
      nameIn, instIn, emailIn, deleteButton)
}

# something still off with NS, end up with contribList-contribList-test 
contribRow_server <- function(id, ...){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # cat(ns("DeleteContrib"))
    # cat(input$DeleteContrib())
    observeEvent(input$DeleteContrib, {
      # insertSelector = paste0("#", id)
      insertSelector = paste0("#", ns("div"))
      # 
      # cat(input[[id]])
      
      removeUI(
        selector = insertSelector#, session = session
      )
      cat(insertSelector)
      # cat(ns())
  })
  })
}

contribList_ui <- function(id,  label, ...){
  ns <- NS(id)
  
  nameIn <- textInput(ns("Name"), "Name")
  instIn <- textInput(ns("Institution"), "Institution")
  emailIn <- textInput(ns("Email"), "Email")
  addContrib <- actionButton(ns("addContrib"), "Add Contributor", icon("plus"), class = "fillWidth")
  div(class = "margin-panel", bs_panel(heading = label, 
           body = div(
             div(class = "inline formGroup",
                 nameIn, instIn, emailIn),
             addContrib))
  )
}

contribList_server <- function(id, ...){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    insertSelector = paste0("#", ns("addContrib"))
    # nsid <- ns("test")
    # ns <- session$ns
    counter <- reactiveVal(0)
    
    observeEvent(input$addContrib, {
      counter(counter() + 1)
      # cat(ns(counter()))
      insertUI(
        selector = insertSelector,
        where = "beforeBegin",
        ui = contribRow_ui(ns(counter()))
      )
      # contribRow_server(ns(counter()))
      contribRow_server(counter())
    })
  })
  
}

contribList_demo <- function() {
  ui <- fluidPage(
    contribRow_ui("contribRowTest"),
    contribList_ui("contribListTest", "contribListTest"))
  server <- function(input, output, session) {
    contribRow_server("contribRowTest")
    contribList_server("contribListTest")
  }
  shinyApp(ui, server)
}


