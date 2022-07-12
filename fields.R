source("utils.R")
source("mods.R")
source("quests.R")

# general panel -----------------------------------------------------------

library(jsonlite)

getPanels <- function(row_n){
  panel_title <- x %>% pluck("panels", row_n, "title")
  panel_id <- x %>% pluck("panels", row_n, "id")
  panel_sections <- x %>% pluck("panels", row_n, "sections")
  # print(length(panel_sections))
  list(panel_title = panel_title, panel_id = panel_id, panel_sections = panel_sections)
  # list(panel_title = panel_title, panel_id = panel_id)
}

x <- read_json("/home/jclark/Work/metaquest/metaquest_fields.json")

xdf <- 1:length(x$panels) %>% 
  map_dfr(getPanels) %>% hoist(panel_sections, section_id = "id", section_fields = "fields") %>% unnest_longer(section_fields) %>% 
  hoist(section_fields, field_id = "id", field_label = "label", field_type = "type") %>% 
  select(panel_title, section_id, field_id, field_label, field_type, field_options = section_fields)

getFieldOpts <- function(field_type, field_options, ...) {
  # print(field_options)
  # thefield %>% str
  # field_info <- field_options %>% pluck("choices")
  # thefield$field_options#$info
  # if(!is.na(field_info)) print(field_info)
  field_note <- ""
  if(isTruthy(field_type)){
    if(field_type %in% c("selectInput", "checkboxGroupInput")){
      field_choices <- field_options %>% pluck("choices") %>% unlist %>% paste(., collapse = ", ")
      field_note <- paste0("Choices: ", field_choices)
    }
    if(field_type == "listInput"){
      subfield_names <- field_options %>% pluck("fields") %>% map_chr("label") %>% paste(., collapse = ", ")
      field_note <- paste0("Subfields: ", subfield_names)
    }
  }

  field_note
}

xdf %>% mutate(field_note = pmap_chr(., getFieldOpts)) %>% select(-field_options)


xdf %>% mutate(field_note = pmap_chr(., getFieldOpts)) %>% select(-field_options, -field_id)
