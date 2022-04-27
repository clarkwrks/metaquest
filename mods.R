
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
  nameIn <- textInput(NS(id, "Name"), "Name")
  instIn <- textInput(NS(id, "Institution"), "Institution")
  deleteButton <- actionButton(NS(id, "DeleteContrib"), "Delete", icon("trash"))
  div(class = "inline formGroup", id = id,
      nameIn, instIn, deleteButton)
}

# something still off with NS, end up with contribList-contribList-test 
contribRow_server <- function(id, ...){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$DeleteContrib, {
      insertSelector = paste0("#", id)
      # 
      removeUI(
        selector = insertSelector,
      )
      cat(insertSelector)
      # cat(ns())
  })
  })
}

contribList_ui <- function(id,  label, ...){
  nameIn <- textInput(NS(id, "Name"), "Name")
  instIn <- textInput(NS(id, "Institution"), "Institution")
  addContrib <- actionButton(NS(id, "addContrib"), "Add Contributor", icon("plus"), class = "fillWidth")
  div(class = "margin-panel", bs_panel(heading = label, 
           body = div(
             div(class = "inline formGroup",
                 nameIn, instIn),
             addContrib))
  )
}

contribList_server <- function(id, ...){
  moduleServer(id, function(input, output, session) {
    insertSelector = paste0("#", NS(id, "addContrib"))
    nsid <- NS(id, "test")
    ns <- session$ns
    observeEvent(input$addContrib, {
      insertUI(
        selector = insertSelector,
        where = "beforeBegin",
        ui = contribRow_ui(nsid)
      )
      contribRow_server(nsid)
    })
  })
  
}



