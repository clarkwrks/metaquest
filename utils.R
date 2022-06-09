
# input infos -------------------------------------------------------------

.tag_validate <- function(tag, name = NULL, class = NULL, ...){
  
  list_attrib <- list(...)
  
  # ensure we have a shiny tag
  if (!inherits(tag, "shiny.tag")){
    stop("tag is not a shiny.tag - tag must be generated using htmltools or shiny")
  }
  
  # if a name argument is provided
  if (!is.null(name)){
    # ensure that the name of the tag is among the provided name(s)
    if (!(tag$name) %in% name){
      stop("tag needs to be one of: ", paste(name, collapse = ", "))
    }
  }
  
  # if class is provided
  if (!is.null(class)){
    class_split <- split_class(class)
    class_observed_split <-
      split_class(htmltools::tagGetAttribute(tag, "class"))
    if (!all(class_split %in% class_observed_split)){
      stop("class is: ", class_observed_split, ", needs to include: ", class_split)
    }
  }
  
  # for each additional attribute
  fn_validate <- function(name, value, tag){
    value_observed <- htmltools::tagGetAttribute(tag, name)
    if (!identical(value_observed, value)){
      stop("attribute ", name, " is: ", value_observed, ", needs to be: ", value)
    }
  }
  
  if (length(list_attrib) > 0){
    purrr::walk2(names(list_attrib), list_attrib, fn_validate, tag)
  }
  
  # make it pipeable (just in case)
  tag
}

# we will be able to get rid of this code
split_class <- function(x) {
  
  x <- stringr::str_trim(x)
  x <- stringr::str_split(x, " ")
  
  x[[1]]
}

# textInputInfo <- function(inputId, label,  ...) {
#   tagx <- textInput(inputId, label)
#   tagx <- .tag_validate(tagx, name = "div", class = "form-group shiny-input-container")
#   infoLink <- actionLink(paste0(inputId, "Info"), icon("info-circle"))
#   infoLink <- htmltools::div(clas = "pull-right", infoLink)
#   tagx$children[[1]] <- tagx$children[[1]] %>% htmltools::tagAppendChild(infoLink)# %>% 
#   # htmltools::tagAppendAttributes(style = "width:100%;")
#   
#   tagx
# }

shinyInput_label_embed <- function(tag, element){
  
  # validate shiny input
  tag <-
    .tag_validate(
      tag,
      name = "div",
      class = "form-group shiny-input-container"
    )
  
  # wrap element in a div that pulls right
  element <- htmltools::div(class = "pull-right", element)
  
  # tag$children[[1]] is a <label/>
  # add element to children, add style attribute
  tag$children[[1]] <-
    tag$children[[1]] %>%
    htmltools::tagAppendChild(element) #%>%
  # htmltools::tagAppendAttributes(style = "width:100%;")
  
  tag
}

textInputInfo <- function(inputId, label, ...) {
  infoID <- paste0(inputId, "Info")
  textInput(inputId, label) %>% shinyInput_label_embed(actionLink(infoID, icon("info-circle")))
}

dateInputInfo <- function(inputId, label, ...) {
  infoID <- paste0(inputId, "Info")
  dateInput(inputId, label) %>% shinyInput_label_embed(actionLink(infoID, icon("info-circle")))
}

selectInputInfo <- function(inputId, label, choices, ...) {
  infoID <- paste0(inputId, "Info")
  selectInput(inputId, label, choices) %>% shinyInput_label_embed(actionLink(infoID, icon("info-circle")))
}

textAreaInputInfo <- function(inputId, label, width, height, resize, ...) {
  infoID <- paste0(inputId, "Info")
  textAreaInput(inputId, label, width=width, height=height, resize=resize) %>% shinyInput_label_embed(actionLink(infoID, icon("info-circle")))
}

# accordion ---------------------------------------------------------------

# removed `bs_set_data(x, parent = .id(id_accordion))` 
# and `id_panel <- paste(id_accordion, n_panel, sep = "-")` -> `id_panel <- override_id`
# attach a radioButtons input with description
# attach status div
# title -> span(icon("plus"), title)
bs_append_noparent_toggle <- function(tag, title, content, override_id, 
                                      condition = NULL, status = TRUE, ...){
  
  # characterize the existing accordion
  n_panel <- length(tag$children)
  panel_type <- attr(tag, "bsplus.panel_type")
  use_heading_link <- attr(tag, "bsplus.use_heading_link")
  
  # get/set id's for constituent elements
  id_accordion <- htmltools::tagGetAttribute(tag, "id")
  # id_panel <- paste(id_accordion, n_panel, sep = "-")  # replace with named panels
  id_panel <- override_id
  id_heading <- paste(id_panel, "heading", sep = "-")
  id_collapse <- paste(id_panel, "collapse", sep = "-")
  
  # function to attach target
  .attach_collapse_local <- function(x){
    x <- bs_attach_collapse(x, id_collapse)
    # x <- bs_set_data(x, parent = .id(id_accordion))
    x <- bs_set_aria(x, expanded = TRUE, controls = id_collapse)
  }
  
  title <- span(icon("plus"), title)
  
  heading <-
    htmltools::tags$div(id = id_heading, class = "panel-heading", role = "tab")
  
  if (use_heading_link){
    
    # attach the collapse to the heading
    heading <- .attach_collapse_local(heading)
    # add some style so that heading appears clickable
    heading <-
      htmltools::tagAppendAttributes(heading, style = "cursor: pointer;")
    panel_title_content <- title
  } else {
    # wrap the title in a link, attach the collapse
    link <- htmltools::tags$a(title)
    link <- .attach_collapse_local(link)
    
    panel_title_content <- link
  }
  
  # compose the panel title
  panel_title <- htmltools::tags$h4(class = "panel-title", panel_title_content)
  # put the panel title into the heading
  heading <- htmltools::tagAppendChild(heading, panel_title)
  if (length(condition) > 0) {
    id_toggle <- paste0(id_panel, "Toggle")
    id_info <- paste0(id_panel, "Info")
    panel_toggle <- radioButtons(id_toggle, condition, 
                                 c("True" = TRUE, "False" = FALSE), selected = character(0), inline = TRUE) %>% 
      shinyInput_label_embed(actionLink(id_info, icon("info-circle")))
    heading <- htmltools::tagAppendChild(heading, panel_toggle)
  }
  if (status == TRUE) {
    id_status <- paste(id_panel, "status", sep = "")
    panel_status <- div(id = id_status, "Status:", class = "panel-status-text")
    heading <- htmltools::tagAppendChild(heading, panel_status)
  }

  # what to do if panel is empty?
  panel_body_style <-
    ifelse(
      identical(length(content), 0L),
      "padding-top: 0px; padding-bottom: 0px;",
      ""
    )
  
  collapse <-
    htmltools::tags$div(
      id = id_collapse,
      class = "panel-collapse collapse",
      role = "tabpanel",
      htmltools::tags$div(
        class = "panel-body",
        style = panel_body_style,
        content
      )
    )
  
  collapse <- bs_set_aria(collapse, labelledby = id_heading)
  
  # if this is the first panel, set it as open (add option to suppress)
  if (identical(n_panel, 0L)){
    collapse <- htmltools::tagAppendAttributes(collapse, class = "in")
  }
  
  # compose the panel
  panel <-
    htmltools::tags$div(class = "panel", id = id_panel, heading, collapse)
  panel <- htmltools::tagAppendAttributes(panel, class = panel_type)
  
  # append panel to accordion
  tag <- htmltools::tagAppendChild(tag, panel)
  
  tag
}


