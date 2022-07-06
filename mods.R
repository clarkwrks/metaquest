# infoInput ---------------------------------------------------------------

infoInput_ui <- function(id, label, type, choices, value = "", info=NA, ...) {
  input_id <- NS(id, "Input")
  x <- switch(type,
              textIn = textInput(input_id, label, value),
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

infoInput_server <- function(id, info=NA, formData = formData, type, ...){
  
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns
    
    observeEvent(input$Input, {
      formData[[ns("Input")]] <- input$Input
    })
    
    observeEvent(formData[[ns("Input")]], {
      # print(paste0(
      #   "Updating ", type, " field: ",
      #   ns("Input"), " = ", formData[[ns("Input")]]
      #   ))
      switch(type,
             textIn =
               updateTextInput(session, "Input",
                               value = formData[[ns("Input")]]),
            selectIn =
               updateSelectInput(session, "Input",
                                 selected = formData[[ns("Input")]]),
             dateIn =
               updateDateInput(session, "Input",
                               value = formData[[ns("Input")]]),
             textareaIn =
               updateTextAreaInput(session, "Input",
                                   value = formData[[ns("Input")]])
      )
      # if(type == "textIn"){
      # #   print("sure")
      # #   xyz <- ns("Input") %>% as.character
      # #   print(xyz)
      # #   # updateTextInput(session, "Input", value = formData[[ns("Input")]])
      #   updateTextInput(session, inputId = "Input", value = "wtf")
      # }
    }
    )
    
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
            ~id,    ~type,        ~label, ~value,
         "name", "textIn",        "Name", "test1",
  "institution", "textIn", "Institution", "test2",
        "email", "textIn",       "Email", "test3"
  )

formListRow_ui <- function(id, fields, ...){
  ns <- NS(id)
  fields_ns <- fields %>% mutate(id = ns(id))
  
  deleteButton <- actionButton(ns("DeleteRow"), "Delete", icon("trash"))
  # print(contrib_row_ns)
  div(class = "inline formGroup formListRow", 
      id = ns("div"),
      fields_ns %>% pmap(infoInput_ui), 
      deleteButton,
      hr())
}

formListRow_server <- function(id, fields, formData=formData, modData, parent_id, ...){
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns

    fields_ns <- fields
    # print(fields_ns)
    fields_ns %>% pmap(infoInput_server, formData=formData)
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
        # .subset2(rv, "impl")$.values$remove(input_id)
        removeReactiveValuesIndex(rv, input_id)
      }
      list_nrow <- paste0(parent_id, "-Nrow")
      fields_ns %>% pmap(deleteRowFormData, rv=formData)
      fields_ns %>% pmap(deleteRowFormData, rv=input)
      fields_ns %>% pmap(deleteRowFormData, rv=modData)
      
      delete_id <- paste0(ns("DeleteRow"))
      # .subset2(formData, "impl")$.values$remove(delete_id)
      # .subset2(input, "impl")$.values$remove(delete_id)
      removeReactiveValuesIndex(formData, delete_id)
      removeReactiveValuesIndex(input, delete_id)
      formData[[list_nrow]] <- formData[[list_nrow]] -1

  })
  })
}

formList_ui <- function(id,  label, ...){
  ns <- NS(id)
  
  addRow <- actionButton(ns("addRow"), "Add Row", icon("plus"), class = "fillWidth")
  div(class = "margin-panel", bs_panel(heading = label, 
           body = div(class = "panel panel-default y-overflow-scroll scroll-shadows",
             addRow))
  )
}

formList_server <- function(id, formData=formData, ...){
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns

    insertSelector = paste0("#", ns("addRow"))
    
    modData <- reactiveValues()
    
    observe({
      formData_rows <- reactiveValuesToList(formData)
      isolate({
        formData_rownames <- formData_rows %>% names %>%
        str_subset(paste0(id, "-\\d+-")) %>%
        str_extract(paste0(id, "-\\d+-")) %>%
        unique
      modData_rownames <- reactiveValuesToList(modData) %>% names %>%
        str_subset(paste0(id, "-\\d+-")) %>%
        str_extract(paste0(id, "-\\d+-")) %>%
        unique

        if(length(modData_rownames) > 0) {
        modData_rownames_mismatch <- formData_rownames[!formData_rownames %in% modData_rownames]
      } else {
        modData_rownames_mismatch <- formData_rownames
      }
      # formData_rownames %>% print
      # modData_rownames %>% print
      if(length(modData_rownames_mismatch) > 0){
        modData_rownames_mismatch <- modData_rownames_mismatch %>% str_sort
        
          for(formData_rowname in 1:length(modData_rownames_mismatch)){
            modData_newrow <- modData_rownames_mismatch[[formData_rowname]]
            # modData_newrow %>% print
            row_number <- modData_newrow %>%
              str_extract("(?<=-)\\d+(?=-)") %>%
              as.numeric
            # modData_newrow %>% print
            new_fields <- formData_rows[formData_rows %>% names %>% str_detect(modData_newrow)]
            # new_fields %>% print
            
            new_row_fields <- contrib_row
            
            for(new_field in 1:length(new_fields)){
              # print(paste0("field: ", new_field, ". ", names(new_fields[new_field])))
              new_field <- new_fields[new_field]
              new_field_name <- names(new_field)
              # new_field[[new_field_name]] %>% print
              modData[[new_field_name]] <- new_field[[new_field_name]]
              new_field_id <- new_field_name %>% str_extract("[a-z]+(?=-Input)")
              new_row_fields[str_detect(new_row_fields$id, new_field_id), "value"] <- new_field[[new_field_name]]
            }
  
              insertUI(
                selector = insertSelector,
                where = "beforeBegin",
                ui = formListRow_ui(ns(row_number), fields = new_row_fields, modData = modData)
              )
              formListRow_server(row_number, fields = new_row_fields, formData = formData, modData = modData, parent_id = id)
            }
      }
      })
      
      # reactivity probably still wrong. need to 
            # reactiveValuesToList(modData) %>% names %>% print
    })
    
    findFreeNumber <- function(current_numbers) {
      x <- current_numbers
      print(paste0("length is ", length(x)))
      if(length(x) >= 1) {
        free_nums <- {min(x):max(x)} %>% .[!. %in% x] %>% first
        if(!is.na(free_nums)) {
          print("found free num")
          return(free_nums)
        } else {
          print("add num")
          return(max(x) + 1)
        }
      } else {
        print("length !>= 1")
        return(1)
      }
    }
    
    observeEvent(input$addRow, {
      # rowDif() %>% print
      free_num <- reactiveValuesToList(formData) %>% names %>% 
        str_subset(paste0(id, "-\\d+-")) %>%
        str_extract(paste0(id, "-\\d+-")) %>%
        unique %>%
        str_extract("(?<=-)\\d+(?=-)") %>%
        as.numeric %>%
        findFreeNumber
      # formData[[ns("Nrow")]] <- formData[[ns("Nrow")]] + 1
      new_rows <- rep("", nrow(contrib_row))  %>% 
        as.list %>% set_names(paste0(
          ns(free_num), 
          "-", 
          contrib_row$id, 
          "-Input")) 
      for(x in 1:length(new_rows)){
        x <- new_rows[x]
        x_name <- names(x)
        formData[[x_name]] <- x[[x_name]]
      }
        # new_rows %>% print
    })
    # 
    # counter <- reactiveVal(0)
    # formData[[ns("Nrow")]] <- 0
    # formListRows <- reactive({
    #   x <- reactiveValuesToList(formData)
    #   x %>% names %>% 
    #     str_subset(paste0(id, "-\\d+-")) %>% 
    #     str_extract(paste0(id, "-\\d+-")) %>%
    #     unique
    # })
    # 
    # rowDif <- reactiveVal(0)
    # observeEvent(formData[[ns("Nrow")]], {
    # 
    #   ui_rows <- formListRows() %>% length
    #   # ui_rows <- isolate(formListRows()) %>% length
    #   formData_rows <- formData[[ns("Nrow")]]
    #   print(paste0(ui_rows, " rows in ui, ", formData_rows, " in data"))
    #   rowDif(formData_rows - ui_rows)
    # })
    # observeEvent(input$addRow, {
    #   # rowDif() %>% print
    #   formData[[ns("Nrow")]] <- formData[[ns("Nrow")]] + 1
    # })
    # 
    # findFreeNumber <- function(current_numbers) {
    #   x <- current_numbers
    #   print(paste0("length is ", length(x)))
    #   if(length(x) >= 1) {
    #     free_nums <- {min(x):max(x)} %>% .[!. %in% x] %>% first
    #     if(!is.na(free_nums)) {
    #       print("found free num")
    #       return(free_nums)
    #     } else {
    #       print("add num")
    #       return(max(x) + 1)
    #     }
    #   } else {
    #     print("length !>= 1")
    #     return(1)
    #   }
    # }
    # 
    # next_free_number <- reactiveVal(0)
    # current_row_ids <- reactiveVal()
    # 
    # observe({
    #   form_ids <- reactiveValuesToList(formData) %>% names %>% 
    #         str_subset(paste0(id, "-\\d+-")) %>%
    #         str_extract(paste0(id, "-\\d+-")) %>%
    #         unique %>%
    #         str_extract("(?<=-)\\d+(?=-)") %>%
    #         as.numeric #%>%
    #   
    #   # print(form_ids)
    #   current_row_ids(form_ids)
    #     #     # findFreeNumber()
    #     #   print(paste0("numlength ", x))
    #     #   # if(x > 0) next_free_number(x)
    # })
    # 
    # # observe({
    # #   x_form <- reactiveValuesToList(formData) %>% names %>% 
    # #     str_subset(paste0(id, "-\\d+-")) %>% 
    # #     str_extract(paste0(id, "-\\d+-")) %>%
    # #     unique
    # #   # print(x_form)
    # #   x <- x_form %>% 
    # #     str_extract("(?<=-)\\d+(?=-)") %>%
    # #     as.numeric #%>%
    # #     # findFreeNumber()
    # #   print(paste0("numlength ", x))
    # #   # if(x > 0) next_free_number(x)
    # # })
    # 
    # observeEvent(rowDif(), {
    #     if(rowDif() > 0){
    #       counter(counter() + 1)
    #       # free_number <- formListRows() %>% 
    #       #   str_extract("(?<=-)\\d+(?=-)") %>%
    #       #   as.numeric %>% findFreeNumber
    #       # next_free_number(free_number)
    #       # next_free_number() %>% print
    #       free_number <- current_row_ids() %>% findFreeNumber
    #       current_row_ids(c(current_row_ids(), free_number))
    #       # current_row_ids() %>% print
    #       # counter() %>% print
    #       insertUI(
    #         selector = insertSelector,
    #         where = "beforeBegin",
    #         ui = formListRow_ui(ns(free_number), fields = contrib_row)
    #       )
    #       formListRow_server(free_number, fields = contrib_row, formData = formData, parent_id = id)
    #       rowDif(rowDif() - 1)
    #   }
    # })
    # 
    
    # formData[[ns("Nrow")]] <- 1
    
    
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


