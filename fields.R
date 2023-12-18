source("utils.R")
# source("mods.R")
# source("quests.R")

library(jsonlite)

# 
# getPanels <- function(panel_n, form_json){
#   panel_title <- form_json %>% pluck("panels", panel_n, "title")
#   panel_id <- form_json %>% pluck("panels", panel_n, "id")
#   panel_sections <- form_json %>% pluck("panels", panel_n, "sections") %>% list
#   # print(length(panel_sections))
#   list(panel_title = panel_title, panel_id = panel_id, panel_sections = panel_sections)
#   # list(panel_title = panel_title, panel_id = panel_id)
# }

# input_ui ----------------------------------------------------------------



checkboxGroupInput_ui <- function(inputId, label, choices = NULL, 
                                  value = NULL, inline = FALSE, 
                                  width = NULL, choiceNames = NULL,  
                                  choiceValues = NULL){
  checkboxGroupInput(inputId, label, choices, selected=value, inline, width, 
                     choiceNames,  choiceValues)
}

checkboxInput_ui <- function(inputId, label, value = FALSE){
  checkboxInput(inputId, label, value)
}

dateInput_ui <- function(inputId, label, value = ""){
  dateInput(inputId, label, value)
}

selectInput_ui <- function(inputId, label, choices, value = NULL, 
                           multiple = FALSE, selectize = TRUE,
                           width = NULL, size = NULL) {
  selectInput(inputId, label, choices, selected=value, multiple, 
              selectize, width, size)
}

selectizeInput_ui <- function(inputId, label, choices, value = "", multiple = FALSE, 
                              options = NULL, width = NULL){
  selectizeInput(inputId, label=label, choices=choices, selected = value,
                 multiple=multiple, options=c(options,dropdownParent = 'body'), width=width)
}

textInput_ui <- function(inputId, label, value = "", width = NULL, placeholder = NULL){
  textInput(inputId, label, value, width, placeholder)
}

textAreaInput_ui <- function(inputId, label, value="", width="fit-content", height="8em", resize="both"){
  textAreaInput(inputId, label, value, width, height, resize = resize)
}

listInput_ui <- function(inputId,  label, info,...){
  ns <- NS(inputId)
  
  info_link <- NULL
  if(isTruthy(info)){
    info_link <- div(class="pull-right", actionLink(ns("Info"), icon("info-circle")))
  }
  addRow <- actionButton(ns("addRow"), "Add Row", icon("plus"), class = "eightyWidth button-secondary")
  div(
    div(class = "control-label", tag("label", label), info_link),
    div(class = "panel panel-default y-overflow-scroll scroll-shadows",
        addRow)
  )
}

listInputRow_ui <- function(id, fields, ...){
  ns <- NS(id)
  # fields_ns <- rowFields %>% mutate(id = ns(id))
  
  deleteButton <- actionButton(ns("DeleteRow"), "Delete", icon("trash"), 
                               style="float:right; margin-right:.5em;", 
                               class = "btn-danger")
  # fields_ns %>% print
  # rowFieldsJson_ns <- rowFieldsJson %>% map(function(x) modify_at(x, "id", function(y) y=ns(y)))
  # rowFieldsJson_ns %>% print
  # fields_ns %>% transpose %>% print
  div(class = "inline formGroup formListRow", 
      id = ns("div"),
      # fields_ns %>% map(~buildField_ui(.x %>% as.list)), 
      # fields_ns %>% transpose %>% map(buildField_ui), 
      fields %>% map(buildField_ui),
      deleteButton)
}

# build_ui ----------------------------------------------------------------


buildField_ui <- function(field, ...) {
  inputId <- NS(field$id, "Input")
  label <- field$label
  type <- paste0(field$type, "_ui")
  
  
  if (type == "listInput_ui") {
    # print(paste("id:", id))
    # print(field)
    input_args <- list(
      inputId = field$id,
      label = field$label,
      info = field$info
    ) %>% c(field$input_options)
    field_out <- do.call(type, input_args)
    
  } else {
    # print(field$input_options)
    input_args <- list(inputId = NS(field$id, "Input"),
                       label = field$label) %>% c(field$input_options)
    
    if (type %in% c("textInput_ui", "textAreaInput_ui")) {
      input_args$value <- field$value
    }
    field_out <- do.call(type, input_args)
    
    if (isTruthy(field$info)) {
      field_out %>%
        shinyInput_label_embed(actionLink(NS(field$id, "Info"), icon("info-circle")))
    } else {
      field_out
    }
  }
  
}

buildSection_ui <- function(section) {
  # list2env(section, environment())
  # section_fields <- fields %>% map(., buildField_ui)
  section_fields <- section$fields %>% map(buildField_ui)
  bs_panel(
    heading = div(class = "section-header", HTML(section$heading)),
    class = "panel-section",
    body = div(
      div(class =  "section-description", HTML(section$description)),
      div(class = "inline form-group", section_fields)
    )
  )
}

buildPanel_ui <- function(panel){
  print(paste("Building panel", panel$id))
  # buildPanel_ui <- function(panel){
  panel_sections_ui <- panel$sections %>% map(buildSection_ui)
  # list(title=panel_title, content=div(panel_sections_ui), override_id=panel_id)
  # panel_sections_ui <- panel$sections %>% map(buildSection_ui)
  list(title = panel$title, content = div(panel_sections_ui), override_id = panel$id, condition=panel$condition)
  # bs_append_noparent_toggle(parent,
  #                           title = panel$title,
  #                           content = panel_sections_ui,
  #                           override_id = panel$override_id,
  #                           condition = panel$condition)
}

buildAccordion_ui <- function(parent, child){
  bs_append_noparent_toggle(parent, 
                            title = child$title, 
                            content = child$content, 
                            override_id = child$override_id,
                            condition = child$condition)
}
buildMetaQuest_ui <- function(form_json){
  
  panels_ui <- form_json$panels %>% map(buildPanel_ui)
  
  main_area_ui <- bs_accordion(id = "mainPanelAccord") %>%
    bs_set_opts(panel_type = "primary", use_heading_link = FALSE)
  
  reduce(panels_ui, buildAccordion_ui, .init = main_area_ui)

}


# input_server ------------------------------------------------------------

fieldInput_server <- function(field, formData = formData, ...){
  id <- field$id
  type <- field$type

    if(type == "listInput") {
    # listInput_server(id = field$id, formData = formData, info = field$info, rowFields = field$fields %>% map_dfr(as_tibble), rowFieldsJson = field$fields)
    listInput_server(field, formData = formData)
    # field$fields %>% map_dfr(as_tibble) %>% print
  } else {
    moduleServer(id, function(input, output, session) {
      id
      ns <- session$ns
      # ns <- NS(id)
      # formData[[ns("ns_field")]] <- ns("")
      # 
      # print(paste0("fieldInput_server id: ", id))
      # print(paste0("fieldInput_server ns: ", ns("")))

      # need to use freezeReactiveValue? on input and formData
      # https://mastering-shiny.org/action-dynamic.html#freezing-reactive-inputs
      
      
      observeEvent(formData[[ns("Input")]], {
        freezeReactiveValue(input, ns("Input"))

        print(paste0(
          "Updating ", type, " field: ",
          ns("Input"), " = ", formData[[ns("Input")]]
          ))

        switch(type,
               textInput =
                 updateTextInput(session, "Input",
                                 value = formData[[ns("Input")]]),
               selectInput =
                 updateSelectInput(session, "Input",
                                   selected = formData[[ns("Input")]]),
               dateInput =
                 updateDateInput(session, "Input",
                                 value = formData[[ns("Input")]]),
               textAreaInput =
                 updateTextAreaInput(session, "Input",
                                     value = formData[[ns("Input")]]),
               checkboxGroupInput =
                 updateCheckboxGroupInput(session, "Input",
                                          selected = formData[[ns("Input")]]),
               selectizeInput = {
                 updated_choices <- c(
                   field$input_options$choices, 
                   formData[[ns("Input")]]) %>% 
                   unique
                 # print(paste0(ns("Input"), " val: ", formData[[ns("Input")]]))
                 # updated_choices %>% str %>% print
                 updateSelectizeInput(session, "Input",
                                      server = TRUE,
                                      choices = updated_choices,
                                      selected = formData[[ns("Input")]])
               }
        )
      })
      # print(ns("Input"))
      observeEvent(input$Input, {
        freezeReactiveValue(formData, ns("Input"))
        formData[[ns("Input")]] <- input$Input
      })
      
      if(isTruthy(field$info)) {
        info <- field$info
        infoModal <- function(content){
          modalDialog(
            HTML(content),
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
      
    })}
}

listInputRow_server <- function(id, rowFields, formData=formData, modData, ...){
  
    moduleServer(id, function(input, output, session) {
    
    rowFields %>% map(fieldInput_server, formData=formData)
      
    id
    ns <- session$ns
    # print(paste0("listInputRow_server ns(id): ", ns("")))
    # formData[[ns("ns_row")]] <- ns("")
    fields_ns <- rowFields

    # row_id <- id
    # list_id <- id
    observeEvent(input$DeleteRow, {
      # insertSelector = paste0("#", id)
      insertSelector = paste0("#", ns("div")) %>% print

      removeUI(
        selector = insertSelector#, session = session
      )

      removeRowFromRV <- function(rv){
        rv_list <- reactiveValuesToList(rv) 
        rv_match <- rv_list %>% names %>%
          str_starts(ns(""))
        rv_row <- rv_list[rv_match] %>% names
        if(length(rv_row)>0){
          for(rv_item in rv_row){
            rv[[rv_item]] <- "delete"
            
            removeReactiveValuesIndex(rv_item, rv)
            # doesn't work from it's own call !?!? eg passing rv=input
            removeReactiveValuesIndex(rv_item, input)
          }
        }
      }
      
      removeRowFromRV(formData) # do work
      # removeRowFromRV(input) # no work
      removeRowFromRV(modData) # don't know if work
      
      ## works:
      delete_id <- paste0(ns("DeleteRow"))
      removeReactiveValuesIndex(delete_id, formData)
      removeReactiveValuesIndex(delete_id, input)
      
    })
  })
}

listInput_server <- function(listField, formData=formData, ...){
  id <- listField$id
  rowFields <- listField$fields
  
  moduleServer(id, function(input, output, session) {
    id
    # print(paste0("listInput_server id: ", id))
    ns <- session$ns
    # formData[[ns("ns_list")]] <- ns("")

    # attach info modal if provided
    if(isTruthy(listField$info)) {
      infoModal <- function(content){
        modalDialog(
          HTML(content),
          easyClose = TRUE,
          footer = tagList(
            modalButton("OK")
          )
        )
      }
      observeEvent(input$Info, {
        showModal(infoModal(listField$info))
      })
    }
    
    # where to insert new rows
    insertSelector <- paste0("#", ns("addRow"))
    
    # like formData, but only for this listInput 
    modData <- reactiveValues()
    observe({
      formData_rows <- reactiveValuesToList(formData)
      # print(id)
      isolate({
            formData_rownames <<- formData_rows %>% names %>%
              str_extract(paste0("(?<=\\b", id, "-)\\d+(?=-)")) %>%
              discard(is.na) %>%
              unique %>% as.numeric
            
            modData_rownames <<- reactiveValuesToList(modData) %>% names %>%
              str_extract(paste0("(?<=\\b", id, "-)\\d+(?=-)")) %>%
              discard(is.na) %>%
              unique %>% as.numeric

           # get list of rownames to be inserted
           if(length(modData_rownames) > 0) {
             toInsert_rownames <- formData_rownames[!formData_rownames %in% modData_rownames]
           } else {
             toInsert_rownames <- formData_rownames
           }
            
            if(length(toInsert_rownames) > 0){
              toInsert_rowname <- toInsert_rownames %>% first
              # str_glue("Inserting {ns(toInsert_rowname)}") %>% print
              
              insertListRow(toInsert_rowname, rowFields)
              addListRow_rv(toInsert_rowname, modData)

            }
        })
      })

    
    
    insertListRow <- function(row_num, rowFields) {
      # ns explicitly for UIs
      # ns automatically for servers
      
      row_num_ns <- ns(row_num)
      rowFields_ns <- rowFields %>% 
        map(function(x) modify_at(x, "id", function(y) y=paste0(row_num_ns, "-", y)))
      
      insertUI(
        selector = insertSelector,
        where = "beforeBegin",
        ui = listInputRow_ui(
          id = row_num_ns,
          rowFields_ns),
        immediate = TRUE
      )
      
      listInputRow_server(row_num, 
                          rowFields = rowFields, 
                          formData = formData, 
                          modData = modData)
    }
    
  
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
    
    addListRow_rv <- function(row_num, target_rv){
      rowFields_ns <- rowFields %>% 
        map(pluck("id")) %>% 
        {str_glue("{ns(row_num)}-{.}-Input")}
      
      for(rowField in rowFields_ns){
        target_rv[[rowField]] <- ""
      }
    }
    
    observeEvent(input$addRow, {
      print("add row")
      addRow_num <- findFreeNumber(formData_rownames)
      str_glue("Adding row {addRow_num} to formData") %>% print
      
      addListRow_rv(addRow_num, formData)
    })
    
    addListRow_rv(1, formData)
    
  })
}

panel_server <- function(id, formData, info, condition, ...){
  moduleServer(id, function(input, output, session){
    id
    if(isTruthy(condition)){
      observe({
        ### got problems. may be related to the shinyjs package needing to call shinyjs::useShinyJS AFTER modules
        ### to revert, use namespacing in bs_append_noparent_toggle
        ns <- session$ns
        
        # x <- ns("Toggle")
        panel_status <- input$Toggle
        # print(paste0(id, "-Toggle"))
        # print(input$Toggle)
        # print(panel_status)
        # print(id)
        toggleCssClass(id, "panel-info", is.null(panel_status), asis=TRUE)
        toggleCssClass(id, "panel-primary", isTRUE(as.logical(panel_status)), asis=TRUE)
        toggleCssClass(id, "panel-disabled", isFALSE(as.logical(panel_status)), asis=TRUE)
      })
    }
    
    panelModal <- function(){
      modalDialog(
        div(
          HTML(info)
        ),
        easyClose = TRUE
      )
    }
    observeEvent(input$Info, {
      showModal(panelModal())
    })
  })
}

# build_server ------------------------------------------------------------

buildField_server <- function(field, formData){

    if(field$type %in% c("textInput", "textAreaInput", "dateInput", "selectInput", "selectizeInput", "checkboxGroupInput")){
    fieldInput_server(field, formData=formData)
  }
  if(field$type == "listInput"){
    listInput_server(id = field$id, formData = formData, info = field$info, rowFields = field$fields %>% map_dfr(as_tibble), rowFieldsJson = field$fields)
  }
}

buildSection_server <- function(section, formData){
  # print(section$id)
  # section$fields %>% map(buildField_server, formData=formData)
  section$fields %>% map(fieldInput_server, formData=formData)
}

buildPanel_server <- function(panel, formData){
  # print(panel$id)
  panel$sections %>% map(buildSection_server, formData = formData)
  panel_server(id = panel$id, formData, session, info = panel$info, condition = panel$condition)
  
}

buildMetaQuest_server <- function(form_json, 
                                  formData=reactiveValues(version = "0.0.1")
                                  ){
  form_json$panels %>% map(buildPanel_server, formData = formData)
}

