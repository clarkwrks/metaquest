source("utils.R")
source("mods.R")
source("quests.R")

# general panel -----------------------------------------------------------


formData <- reactiveValues(version = "0.0.2")

preparer_section_fields <- metaquests %>% filter(panel == "general" & section == "preparer")

preparer_section_ui <- bs_panel(heading = "About the metadata preparer", class="panel-section",
                             body = div(class = "inline form-group",
                                        preparer_section_fields %>%
                                          pmap(infoInput_ui)))

# preparer_section_server <- preparer_section_fields %>% pmap(infoInput_server, formData=formData)
# section_server <- function(fields, formData){
#   fields %>% pmap(infoInput_server, formData=formData)
# }
# 
# preparer_section_fields %>% section_server(formData)