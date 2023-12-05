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
                                  selected = NULL, inline = FALSE, 
                                  width = NULL, choiceNames = NULL,  
                                  choiceValues = NULL){
  checkboxGroupInput(inputId, label, choices, selected, inline, width, 
                     choiceNames,  choiceValues)
}

checkboxInput_ui <- function(inputId, label, value = FALSE){
  checkboxInput(inputId, label, value)
}

dateInput_ui <- function(inputId, label, value = ""){
  dateInput(inputId, label, value)
}

selectInput_ui <- function(inputId, label, choices, selected = NULL, 
                           multiple = FALSE, selectize = TRUE,
                           width = NULL, size = NULL) {
  selectInput(inputId, label, choices, selected, multiple, 
              selectize, width, size)
}

selectizeInput_ui <- function(inputId, label, choices, multiple = FALSE, 
                              options = NULL, width = NULL){
  selectizeInput(inputId, label=label, choices=choices, multiple=multiple, options=c(options,dropdownParent = 'body'), width=width)
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

# listInputRow_ui <- function(id, rowFields, rowFieldsJson, ...){
#   ns <- NS(id)
#   fields_ns <- rowFields %>% mutate(id = ns(id))
#   
#   deleteButton <- actionButton(ns("DeleteRow"), "Delete", icon("trash"), 
#                                style="float:right; margin-right:.5em;", 
#                                class = "btn-danger")
#   # fields_ns %>% print
#   rowFieldsJson_ns <- rowFieldsJson %>% map(function(x) modify_at(x, "id", function(y) y=ns(y)))
#   rowFieldsJson_ns %>% print
#   # fields_ns %>% transpose %>% print
#   div(class = "inline formGroup formListRow", 
#       id = ns("div"),
#       # fields_ns %>% map(~buildField_ui(.x %>% as.list)), 
#       # fields_ns %>% transpose %>% map(buildField_ui), 
#       rowFieldsJson_ns %>% map(buildField_ui),
#       deleteButton)
# }
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
# 
# fieldInput_server <- function(id, info=NA, formData = formData, type, ...){
# 
#   moduleServer(id, function(input, output, session) {
#     id
#     ns <- session$ns
#     # need to use freezeReactiveValue? on input and formData
#     # https://mastering-shiny.org/action-dynamic.html#freezing-reactive-inputs
# 
#     # observe({
#     #   # input$Input
#     #   invalidateLater(4000, session)
#     #   # freezeReactiveValue(input, ns("Input"))
#     #   # formData[[ns("Input")]] <- input$Input
#     #   # formData[[ns("Input")]] <- isolate(input$Input)
#     #   isolate({
#     #   formData[[ns("Input")]] <- input$Input
#     #   })
#     # })
#     # observe({
#     #   invalidateLater(4000, session)
#     #   isolate({
#     #   new_val <- formData[[ns("Input")]]
#     #   # freezeReactiveValue(formData, ns("Input"))
#     # 
#     #   print(paste0(
#     #     "Updating ", type, " field: ",
#     #     ns("Input"), " = ", formData[[ns("Input")]]
#     #   ))
#     # 
#     #   switch(type,
#     #          textInput =
#     #            updateTextInput(session, "Input",
#     #                            value = formData[[ns("Input")]]),
#     #          selectInput =
#     #            updateSelectInput(session, "Input",
#     #                              selected = formData[[ns("Input")]]),
#     #          dateInput =
#     #            updateDateInput(session, "Input",
#     #                            value = formData[[ns("Input")]]),
#     #          textAreaInput =
#     #            updateTextAreaInput(session, "Input",
#     #                                value = formData[[ns("Input")]]),
#     #          selectizeInput =
#     #            updateSelectizeInput(session, "Input",
#     #                                 choices = formData[[ns("Input")]],
#     #                                 selected = formData[[ns("Input")]]),
#     #          checkboxGroupInput =
#     #            updateCheckboxGroupInput(session, "Input",
#     #                                     selected = formData[[ns("Input")]])
#     #   )
#     #   })
#     #   }
#     # )
#     observeEvent(formData[[ns("Input")]], {
#       freezeReactiveValue(input, ns("Input"))
# 
#       # print(paste0(
#       #   "Updating ", type, " field: ",
#       #   ns("Input"), " = ", formData[[ns("Input")]]
#       #   ))
# 
#       switch(type,
#              textInput =
#                updateTextInput(session, "Input",
#                                value = formData[[ns("Input")]]),
#              selectInput =
#                updateSelectInput(session, "Input",
#                                  selected = formData[[ns("Input")]]),
#              dateInput =
#                updateDateInput(session, "Input",
#                                value = formData[[ns("Input")]]),
#              textAreaInput =
#                updateTextAreaInput(session, "Input",
#                                    value = formData[[ns("Input")]]),
#              selectizeInput = {
#                # print(choices)
#                # arguments <- list(...)
#                # print(input_options)
#                # mc <- match.call(expand.dots = FALSE)
#                # print(mc$...)
#                # print(deparse(substitute(...)))
#                # testprint <- sapply(substitute(list(...))[-1], deparse)
#                # print(testprint(...))
#                # print(names(list(...)))
#                # rlang::dots_list(..., .preserve_empty = FALSE) %>% print
#                # try(print(input_options))
#                # print(ns("test"))
#                # dots <- enquos(..., .named = TRUE)
#                # names(dots) %>% print
#                # dots$input_options %>% print
#           
#                updateSelectizeInput(session, "Input",
#                                     # server = TRUE,
#                                     choices = formData[[ns("Input")]],
#                                     selected = formData[[ns("Input")]])
#                },
#              checkboxGroupInput =
#                updateCheckboxGroupInput(session, "Input",
#                                         selected = formData[[ns("Input")]])
#       )
#     }
#     )
#     
#     observeEvent(input$Input, {
#       freezeReactiveValue(formData, ns("Input"))
#       print(paste0("Field input observer: ",  ns("Input")))
#       formData[[ns("Input")]] <- input$Input
#     })
#     
#     if(isTruthy(info)) {
#       infoModal <- function(content){
#         modalDialog(
#           HTML(content),
#           easyClose = TRUE,
#           footer = tagList(
#             modalButton("OK")
#           )
#         )
#       }
#       observeEvent(input$Info, {
#         showModal(infoModal(info))
#       })
#     }
#     
#   })
# }

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
                 print(paste0(ns("Input"), " val: ", formData[[ns("Input")]]))
                 updated_choices %>% str %>% print
                 updateSelectizeInput(session, "Input",
                                      server = TRUE,
                                      choices = updated_choices,
                                      selected = formData[[ns("Input")]])
               }
        )
      })
      
      observeEvent(input$Input, {
        freezeReactiveValue(formData, ns("Input"))
        print(paste0("Field input observer ns: ",  ns("Input")))
        print(paste0("Field input observer input: ", input$Input))
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
# 
# listInputRow_server <- function(id, rowFields, formData=formData, modData, parent_id, rowFieldsJson, ...){
#   moduleServer(id, function(input, output, session) {
#     id
#     ns <- session$ns
#     
#     fields_ns <- rowFields
#     # print(fields_ns)
#     # print(rowFieldsJson)
#     
#     # fields_ns %>% pmap(fieldInput_server, formData=formData, fields = rowFields)
#     # rowFields %>% map(fieldInput_server, formData=formData)
#     rowFieldsJson %>% map(fieldInput_server, formData=formData)
#     
#     row_id <- id
#     list_id <- id
#     observeEvent(input$DeleteRow, {
#       # insertSelector = paste0("#", id)
#       insertSelector = paste0("#", ns("div"))
#       
#       removeUI(
#         selector = insertSelector#, session = session
#       )
#       deleteRowFormData <- function(id, rv, ...){
#         input_id <- paste0(ns(id), "-Input")
#         # .subset2(rv, "impl")$.values$remove(input_id)
#         removeReactiveValuesIndex(rv, input_id)
#       }
#       list_nrow <- paste0(parent_id, "-Nrow")
#       fields_ns %>% pmap(deleteRowFormData, rv=formData)
#       fields_ns %>% pmap(deleteRowFormData, rv=input)
#       fields_ns %>% pmap(deleteRowFormData, rv=modData)
#       
#       delete_id <- paste0(ns("DeleteRow"))
#       # .subset2(formData, "impl")$.values$remove(delete_id)
#       # .subset2(input, "impl")$.values$remove(delete_id)
#       removeReactiveValuesIndex(formData, delete_id)
#       removeReactiveValuesIndex(input, delete_id)
#       formData[[list_nrow]] <- formData[[list_nrow]] -1
#       
#     })
#   })
# }


listInputRow_server <- function(id, rowFields, formData=formData, modData, parent_id, rowFieldsJson, ...){
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns
  
    fields_ns <- rowFields
    # print(fields_ns)
    # print(rowFieldsJson)
    
    # fields_ns %>% pmap(fieldInput_server, formData=formData, fields = rowFields)
    # rowFields %>% map(fieldInput_server, formData=formData)
    rowFieldsJson %>% map(fieldInput_server, formData=formData)
    
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

listInput_server <- function(listField, formData=formData, ...){
  id <- listField$id
  rowFields <- listField$fields
  
  moduleServer(id, function(input, output, session) {
    id
    ns <- session$ns
    
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
        listField$id %>% print
        listField$fields %>% length %>% print
      })
    }
    
    # where to insert new rows
    insertSelector <- paste0("#", ns("addRow"))
    
    # like formData, but only for this listInput 
    modData <- reactiveValues()
    
    
    # observe({
    #   formData_rows <- reactiveValuesToList(formData)
    #   isolate({
    #     
    #     # get rownames in formData (may not be rendered yet)
    #     formData_rownames <- formData_rows %>% names %>%
    #       str_subset(paste0("^", id, "-\\d+-")) %>%
    #       str_extract(paste0("^", id, "-\\d+-")) %>%
    #       unique
    #     
    #     # get rownames in modData (currently rendered)
    #     modData_rownames <- reactiveValuesToList(modData) %>% names %>%
    #       str_subset(paste0("^", id, "-\\d+-")) %>%
    #       str_extract(paste0("^", id, "-\\d+-")) %>%
    #       unique
    #     
    #     # get list of rownames to be inserted
    #     if(length(modData_rownames) > 0) {
    #       toInsert_rownames <- formData_rownames[!formData_rownames %in% modData_rownames]
    #     } else {
    #       toInsert_rownames <- formData_rownames
    #     }
    #     
    #     toInsert_rownames %>% print()
    #     
    #     # if any formData rows missing from modData, insert them
    #     if(length(toInsert_rownames) > 0){
    #       # sort by rownumber
    #       toInsert_rownames <- toInsert_rownames %>% str_sort
    #       
    #       # loop through rows to insert
    #       for(toInsert_rowindex in 1:length(toInsert_rownames)){
    #         toInsert_rowname <- toInsert_rownames[[toInsert_rowindex]]
    #         print(paste("toInsert_rowindex = ", toInsert_rowindex))
    #         print(paste("toInsert_rowname = ", toInsert_rowname))
    #         
    #         # get toInsert_fields values from formData
    #         toInsert_fields <- formData_rows[formData_rows %>% names %>% str_detect(paste0("^", toInsert_rowname))]
    #         # row field defs from json
    #         toInsert_fields_template <- rowFields
    #         # toInsert_fields_template <- rowFieldsJson
    #         
    #         for(new_field in 1:length(toInsert_fields)){
    #           print(paste0("field: ", new_field, ". ", names(toInsert_fields[new_field])))
    #           new_field <- toInsert_fields[new_field]
    #           new_field_name <- names(new_field)
    #           modData[[new_field_name]] <- new_field[[new_field_name]]
    #           new_field_id <- new_field_name %>% str_extract("[a-z]+(?=-Input)")
    #           paste0("new field name: ", new_field_name) %>% print
    #           paste0("new field id: ", new_field_id) %>% print
    #           toInsert_fields_template[str_detect(toInsert_fields_template$id, new_field_id), "value"] <- new_field[[new_field_name]]
    #         }
    #         
    #         toInsert_rownumber <- toInsert_rowname %>%
    #           str_extract("(?<=-)\\d+(?=-)") %>%
    #           as.numeric
    #         
    #         insertUI(
    #           selector = insertSelector,
    #           where = "beforeBegin",
    #           ui = listInputRow_ui(
    #             ns(toInsert_rownumber), 
    #             rowFields = toInsert_fields_template, 
    #             rowFieldsJson = rowFieldsJson, 
    #             modData = modData)
    #         )
    #         # print(id)
    #         listInputRow_server(toInsert_rownumber, rowFields = toInsert_fields_template, formData = formData, modData = modData, parent_id = id, rowFieldsJson = rowFieldsJson)
    #       }
    #     }
    #   })
    # })
    # 
    
    # init_rows <- rep("", length(rowFields))  %>%
    #   as.list %>% set_names(paste0(
    #     ns("1"),
    #     "-",
    #     rowFields$id,
    #     "-Input"))
    
    insertListRow <- function(row_num, fields) {
      row_id <- ns(row_num)
      row_fields <- fields %>% map(function(x) modify_at(x, "id", function(y) y=paste0(row_id, "-", y)))
      insertUI(
        selector = insertSelector,
        where = "beforeBegin",
        ui = listInputRow_ui(
          id = row_id,
          row_fields),
        immediate = TRUE
      )
      # listInputRow_server(toInsert_rownumber, rowFields = toInsert_fields_template, formData = formData, modData = modData, parent_id = id, rowFieldsJson = rowFieldsJson)
      
      
    }
    
    # insertListRow(1, rowFields)
    map(1:10, insertListRow, fields = rowFields)
    
    

    


    # init_rows <- rep("", nrow(rowFields))  %>%
    #   as.list %>% set_names(paste0(
    #     ns("1"),
    #     "-",
    #     rowFields$id,
    #     "-Input"))
    
    # for(x in 1:length(init_rows)){
    #   x <- init_rows[x]
    #   x_name <- names(x)
    #   formData[[x_name]] <- x[[x_name]]
    # }
    # 
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
    
    # observeEvent(input$addRow, {
    #   # rowDif() %>% print
    #   print("add row")
    #   free_num <- reactiveValuesToList(formData) %>% names %>% 
    #     str_subset(paste0(id, "-\\d+-")) %>%
    #     str_extract(paste0(id, "-\\d+-")) %>%
    #     unique %>%
    #     str_extract("(?<=-)\\d+(?=-)") %>%
    #     as.numeric %>%
    #     findFreeNumber
    #   
    #   # formData[[ns("Nrow")]] <- formData[[ns("Nrow")]] + 1
    #   new_rows <- rep("", nrow(rowFields))  %>% 
    #     as.list %>% set_names(paste0(
    #       ns(free_num), 
    #       "-", 
    #       rowFields$id, 
    #       "-Input")) 
    #   for(x in 1:length(new_rows)){
    #     x <- new_rows[x]
    #     x_name <- names(x)
    #     # x_name %>% print
    #     formData[[x_name]] <- x[[x_name]]
    #   }
    # })
    
  })
}

 # listInput_server <- function(id, formData=formData, info=NULL, rowFields, rowFieldsJson, ...){
 #  moduleServer(id, function(input, output, session) {
 #    id
 #    ns <- session$ns
 # 
 #    if(isTruthy(info)) {
 #      infoModal <- function(content){
 #        modalDialog(
 #          HTML(content),
 #          easyClose = TRUE,
 #          footer = tagList(
 #            modalButton("OK")
 #          )
 #        )
 #      }
 #      observeEvent(input$Info, {
 #        showModal(infoModal(info))
 #      })
 #    }
 # 
 #    insertSelector = paste0("#", ns("addRow"))
 # 
 #    # like formData, but only for this listInput
 #    modData <- reactiveValues()
 # 
 #    observe({
 #      formData_rows <- reactiveValuesToList(formData)
 #      isolate({
 # 
 #        # get rownames in formData (may not be rendered yet)
 #        formData_rownames <- formData_rows %>% names %>%
 #          str_subset(paste0("^", id, "-\\d+-")) %>%
 #          str_extract(paste0("^", id, "-\\d+-")) %>%
 #          unique
 # 
 #        # get rownames in modData (currently rendered)
 #        modData_rownames <- reactiveValuesToList(modData) %>% names %>%
 #          str_subset(paste0("^", id, "-\\d+-")) %>%
 #          str_extract(paste0("^", id, "-\\d+-")) %>%
 #          unique
 # 
 #        # get list of rownames to be inserted
 #        if(length(modData_rownames) > 0) {
 #          toInsert_rownames <- formData_rownames[!formData_rownames %in% modData_rownames]
 #        } else {
 #          toInsert_rownames <- formData_rownames
 #        }
 # 
 #        toInsert_rownames %>% print()
 # 
 #        # if any formData rows missing from modData, insert them
 #        if(length(toInsert_rownames) > 0){
 #          # sort by rownumber
 #          toInsert_rownames <- toInsert_rownames %>% str_sort
 # 
 #          # loop through rows to insert
 #          for(toInsert_rowindex in 1:length(toInsert_rownames)){
 #            toInsert_rowname <- toInsert_rownames[[toInsert_rowindex]]
 #            print(paste("toInsert_rowindex = ", toInsert_rowindex))
 #            print(paste("toInsert_rowname = ", toInsert_rowname))
 # 
 #            # get toInsert_fields values from formData
 #            toInsert_fields <- formData_rows[formData_rows %>% names %>% str_detect(paste0("^", toInsert_rowname))]
 #            # row field defs from json
 #            toInsert_fields_template <- rowFields
 #            # toInsert_fields_template <- rowFieldsJson
 # 
 #            for(new_field in 1:length(toInsert_fields)){
 #              print(paste0("field: ", new_field, ". ", names(toInsert_fields[new_field])))
 #              new_field <- toInsert_fields[new_field]
 #              new_field_name <- names(new_field)
 #              modData[[new_field_name]] <- new_field[[new_field_name]]
 #              new_field_id <- new_field_name %>% str_extract("[a-z]+(?=-Input)")
 #              paste0("new field name: ", new_field_name) %>% print
 #              paste0("new field id: ", new_field_id) %>% print
 #              toInsert_fields_template[str_detect(toInsert_fields_template$id, new_field_id), "value"] <- new_field[[new_field_name]]
 #            }
 # 
 #            toInsert_rownumber <- toInsert_rowname %>%
 #              str_extract("(?<=-)\\d+(?=-)") %>%
 #              as.numeric
 # 
 #            insertUI(
 #              selector = insertSelector,
 #              where = "beforeBegin",
 #              ui = listInputRow_ui(
 #                ns(toInsert_rownumber),
 #                rowFields = toInsert_fields_template,
 #                rowFieldsJson = rowFieldsJson,
 #                modData = modData)
 #            )
 #            # print(id)
 #            listInputRow_server(toInsert_rownumber, rowFields = toInsert_fields_template, formData = formData, modData = modData, parent_id = id, rowFieldsJson = rowFieldsJson)
 #          }
 #        }
 #      })
 #    })
 # 
 #    init_rows <- rep("", nrow(rowFields))  %>%
 #      as.list %>% set_names(paste0(
 #        ns("1"),
 #        "-",
 #        rowFields$id,
 #        "-Input"))
 #    for(x in 1:length(init_rows)){
 #      x <- init_rows[x]
 #      x_name <- names(x)
 #      formData[[x_name]] <- x[[x_name]]
 #    }
 # 
 #    findFreeNumber <- function(current_numbers) {
 #      x <- current_numbers
 #      print(paste0("length is ", length(x)))
 #      if(length(x) >= 1) {
 #        free_nums <- {min(x):max(x)} %>% .[!. %in% x] %>% first
 #        if(!is.na(free_nums)) {
 #          print("found free num")
 #          return(free_nums)
 #        } else {
 #          print("add num")
 #          return(max(x) + 1)
 #        }
 #      } else {
 #        print("length !>= 1")
 #        return(1)
 #      }
 #    }
 # 
 #    observeEvent(input$addRow, {
 #      # rowDif() %>% print
 #      print("add row")
 #      free_num <- reactiveValuesToList(formData) %>% names %>%
 #        str_subset(paste0(id, "-\\d+-")) %>%
 #        str_extract(paste0(id, "-\\d+-")) %>%
 #        unique %>%
 #        str_extract("(?<=-)\\d+(?=-)") %>%
 #        as.numeric %>%
 #        findFreeNumber
 # 
 #      # formData[[ns("Nrow")]] <- formData[[ns("Nrow")]] + 1
 #      new_rows <- rep("", nrow(rowFields))  %>%
 #        as.list %>% set_names(paste0(
 #          ns(free_num),
 #          "-",
 #          rowFields$id,
 #          "-Input"))
 #      for(x in 1:length(new_rows)){
 #        x <- new_rows[x]
 #        x_name <- names(x)
 #        # x_name %>% print
 #        formData[[x_name]] <- x[[x_name]]
 #      }
 #    })
 # 
 #  })
# }

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

buildInput_server <- function(input, formData){
  
}

# buildField_server <- function(field, formData){
#   # print(field$type)
#   if(field$type %in% c("textInput", "textAreaInput", "dateInput", "selectInput", "selectizeInput", "checkboxGroupInput")){
#     fieldInput_server(id = field$id, info=field$info, type=field$type, formData=formData)
#   }
#   if(field$type == "listInput"){
#     listInput_server(id = field$id, formData = formData, info = field$info, rowFields = field$fields %>% map_dfr(as_tibble), rowFieldsJson = field$fields)
#     # field$fields %>% map_dfr(as_tibble) %>% print
#   }
# }


buildField_server <- function(field, formData){
  # print(field$type)
  if(field$type %in% c("textInput", "textAreaInput", "dateInput", "selectInput", "selectizeInput", "checkboxGroupInput")){
    fieldInput_server(field, formData=formData)
  }
  if(field$type == "listInput"){
    listInput_server(id = field$id, formData = formData, info = field$info, rowFields = field$fields %>% map_dfr(as_tibble), rowFieldsJson = field$fields)
    # field$fields %>% map_dfr(as_tibble) %>% print
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

# testFun <- function(id, title){
#   print(id)
#   print(title)
# }


# demo --------------------------------------------------------------------

# test_fields <- read_json("metaquest_fields.json")
# test_fields$panels %>% map(~ testFun(.x$id, .x$title))

# test_fields %>% buildMetaQuest_server

# test_fields %>% buildMetaQuest_ui

# testListFun <- function(dfrowlist){
#   print(dfrowlist$id)
# }

# listInput originally designed to accept tibble of rowFields, rework has lists. 
# tried converting to tibble then back for new ui builders 
# something fucky
# test_fields$panels[[1]]$sections[[2]]$fields[[7]]$fields %>% map_dfr(as_tibble) %>% map(~testListFun(.x %>% as.list))
# 
# test_fields$panels[[1]]$sections[[2]]$fields[[7]]$fields %>% map_dfr(as_tibble) %>% .[1,] %>% as.list
# 
# test_fields$panels[[1]]$sections[[2]]$fields[[7]]$fields %>% map_dfr(as_tibble) %>% as.list
# test_fields$panels[[1]]$sections[[2]]$fields[[7]]$fields %>% map_dfr(as_tibble) %>% transpose
# 
# buildMetaQuest_demo <- function() {
#   ui <- fluidPage(
#     test_fields %>% buildMetaQuest_ui()
#   )
#   server <- function(input, output, session) {
#     formData <- reactiveValues(version = "0.0.1")
#     test_fields %>% buildMetaQuest_server(formData = formData)
#   }
#   shinyApp(ui, server)
# }
# buildMetaQuest_demo()
