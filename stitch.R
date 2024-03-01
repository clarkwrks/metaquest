# Walk through metaquest_fields like fields.R, but output html elements instead of Shiny inputs
# Match field ID's to user generated JSON to pull values
# Need to capture deprecated/unmatched user input

metaquest_fields <- read_json("metaquest_fields.json")
metaquest_version <- "0.8.6"
# panels
# sections
# fields
# list fields
# pluck
# keep
metaquest_fields$panels %>% map("id")
metaquest_fields$panels %>% map(pluck("id"))

getPanelIDs <- function(fields_json) {
  panel_ids <- fields_json$panels %>% 
    # map(pluck(c("id", "sections")))
  map(pluck("id"))
}
getPanelIDs(metaquest_fields) %>% print


metaquest_fields$panels %>% bind_rows()

# need to grab id and parent id across tree...
# modify_tree

metaquest_fields$panels %>% modify_depth(1, "id")

input_fields <- read_json("test_data/CatherineDestrempes_metaquest_2024-01-30_15-41-00 (6).json")

x <- metaquest_fields %>% unlist
metaquest_fields_id <- x[str_ends(x %>% names, ".id")] %>% paste0("-Input")

# works, but now needs to be listField aware or agnostic
input_fields[!names(input_fields) %in% metaquest_fields_id] %>% names
input_fields[names(input_fields) %in% metaquest_fields_id] %>% names
