x <- list(blah = 1, blue = 2)

for(y in 1:length(x)){
  print(names(x)[[y]])
  print(x[[y]])
}


x <- jsonlite::read_json("/home/jclark/Downloads/test_blank.json")#, flatten = TRUE) %>% unlist# %>% class
xr <- rjson::fromJSON(file = "/home/jclark/Downloads/test_blank.json")#
list()

test_data <- list()
test_data$date <- as.Date("2022-06-14")
x[str_detect(names(x), "-Nrow")]

xr[[str_detect(names(xr), "-Nrow")]]


list_inputs <- x#[str_detect(names(x), "-Nrow")]
for(i in 1:length(list_inputs)){
  print(list_inputs[[i]])
  print(names(list_inputs)[i])
  # print(asdf[i])
}

# pattern formList-#-field-Input

test_import_data <- list(
  `blah` = "nono",
  `contribList-Nrow` = 2,
  `contribList-1-name-Input` = "Test User 1",
  `contribList-1-email-Input` = "email1",
  `contribList-2-name-Input` = "Test User 2",
  `contribList-2-email-Input` = "email2",
  `otherList-Nrow` = 2,
  `otherList-1-name-Input` = "Test User 1",
  `otherList-1-email-Input` = "email1",
  `otherList-2-name-Input` = "Test User 2",
  `otherList-2-email-Input` = "email2"
)

test_form_data <- list(
  `contribList-Nrow` = 2,
  `contribList-2-name-Input` = "Test User 2",
  `contribList-2-email-Input` = "email2",
  `contribList-4-name-Input` = "Test User 4",
  `contribList-4-email-Input` = "email4",
  `otherList-Nrow` = 2,
  `otherList-6-name-Input` = "Test User 6",
  `otherList-6-email-Input` = "email6",
  `otherList-8-name-Input` = "Test User 8",
  `otherList-8-email-Input` = "email8"
)

import_data_names <- names(test_import_data)
list_inputs <- test_import_data[str_detect(import_data_names, "-Nrow")]

for(x in 1:length(list_inputs)){
  list_nrow <- as.numeric(list_inputs[[x]])
  list_nrow_name <- names(list_inputs)[x]
  list_name <- list_nrow_name %>% str_remove("-Nrow")

  import_list_rows <- names(test_import_data) %>% 
    str_extract(str_c(list_name, "-\\d+-")) %>% 
    na.omit %>% unique
  form_list_rows <- names(test_form_data) %>% 
    str_extract(str_c(list_name, "-\\d+-")) %>% 
    na.omit %>% unique

  # if(formData[[list_name]] < list_nrow){
    # formData[[list_name]] <- list_nrow}

  for(x in 1:length(import_list_rows)){
    import_data_names <- str_replace(import_data_names, import_list_rows[[x]], form_list_rows[[x]])
  }
  }
mod_data <- set_names(test_import_data, import_data_names)

post_data <- test_form_data


valid_inputs <- mod_data[str_detect(names(mod_data), "-Input")]
for(x in 1:length(valid_inputs)){
  input_name <- names(valid_inputs)[[x]]
  
  post_data[[input_name]] <- mod_data[[x]]
}

  # loop through list inputs, extract all rows, rename to existing row numbers
  


test_data$date <- as.Date("2022-06-14")


x <- c(1, 3, 4, 10)
x <- c(1, 2, 3)

"contribList1-2-" %>% str_extract("(?<=-)\\d+(?=-)") %>% as.numeric


test_vals <- list(
  `contribList-1-email-Input` = "email1",
  `contribList-1-institution-Input` = "inst1",
  `contribList-1-name-Input` = "name1",
  `contribList-2-email-Input` = "email2",
  `contribList-2-institution-Input` = "inst2",
  `contribList-2-name-Input` = "name2"
) 

test_vals %>% map(pluck, "contribList-1-email-Input")
test_vals %>% pluck("contribList-1-email-Input")
test_vals %>% pluck("contribList-1-")

row_prefix <- "contribList-1-"
test_vals[test_vals %>% names %>% str_detect(row_prefix)]

contrib_row %>% pull(label) %>% 
  # paste0(row_prefix, .) %>% 
  as.list %>% set_names(paste0(row_prefix, contrib_row$id, "-Input"))# %>%

rep("", nrow(contrib_row))  %>% 
  as.list %>% set_names(paste0(row_prefix, contrib_row$id, "-Input"))

y <- list()
for(x in 1:length(test_vals)){
  # print(names(x))
  x <- test_vals[x]
  x_name <- names(x)
  print(x %>% flatten)
  # print(x_name)
  y[[x_name]] <- x[[x_name]]
}
y

test_vals <- list(
  `contribList-1-email-Input` = "",
  `contribList-1-institution-Input` = "",
  `contribList-1-name-Input` = "",
  `contribList-2-email-Input` = "",
  `contribList-2-institution-Input` = "",
  `contribList-2-name-Input` = ""
) 

row_prefix <- "contribList-1-"
y <- test_vals[test_vals %>% names %>% str_detect(row_prefix)]
test_contrib_row <- contrib_row
for(x in 1:nrow(test_contrib_row)){
  xrow <- test_contrib_row[x, ]
  print(xrow)
  ymatch <- y[names(y) %>% str_detect(xrow$id)]
  test_contrib_row[x, "value"] <- ymatch
}
test_contrib_row

# rlang::are_na
# rlang::missing



ui <- fluidPage(
  p("The first checkbox group controls the second"),
  checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                     c("Item A", "Item B", "Item C")),
  checkboxGroupInput("inCheckboxGroup2", "Input checkbox 2",
                     c("Item A", "Item B", "Item C"))
)

server <- function(input, output, session) {
  observe({
    x <- input$inCheckboxGroup
    
    # Can use character(0) to remove all choices

    # Can also set the label and selec    # if (is.null(x))
    #   x <- character(0)
    # t items
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             selected = x
    )
  })
}

shinyApp(ui, server)



testfieldjson <- read_json("metaquest_fields.json")

testfieldsjson <- testfieldjson$panels[[1]]$sections[[2]]$fields[[7]]$fields

testfieldsjson %>% assign_in()

testfieldsjson %>% pluck(1)

testfieldsjson_ids <- testfieldsjson %>% map("id")
target_field <- testfieldsjson_ids %>% str_detect("name")

testfieldsjson %>% assign_in()

testfieldsjson[target_field][[1]]$value <- "test"

testfieldsjson[testfieldsjson_ids %>% str_detect("name")][[1]]$blah <- "testblah"


testfieldjson$panels[[6]]$sections[[1]]$fields[[7]]$fields

test_list <- list("external_resources-1-external_title-Input",
                 "external_resources-1-external_link-Input",
                  "sources-1-source_license-Input",
                 "sources-1-source_location-Input")
test_string <- "sources"
str_detect(test_list, paste0("^", test_string))


test_list <- list( "a"=c(1,2,3,4), "b"=c(2,4,5), "c"=c(9,5,3,2))
test_select <- c("a", "c")

test_id <- "proj_contributors"

test_list <- list("proj_contributors-1-affiliation-Input"=c(1,2,3,4), 
                  "proj_contributors-1-name-Input"=c(2,4,5), 
                  "proj_contributors-2-affiliation-Input"=c(9,5,3,2),
                  "spatial_data-1-spatial_projection-Input" =c(999,-1))

test_id <- "proj"
test_list <- list("proj-1-affiliation-Input"=c(1,2,3,4), 
                  "proj-1-name-Input"=c(2,4,5), 
                  "proj-2-affiliation-Input"=c(9,5,3,2),
                  "projectors-2-affiliation-Input"=c(9,5,3,2),
                  "reproj-2-affiliation-Input"=c(9,5,3,2),
                  "pro-2-affiliation-Input"=c(9,5,3,2),
                  "spatial_data-1-spatial_projection-Input" =c(999,-1))


test_list %>% keep(names(.) %in% test_id)
test_list %>% keep(names(.) %>% str_detect(paste0("^", test_id, "-")))


testlist  <- list(list("test"=NULL,"id"=NULL), list("test"=NULL,"id"=NULL),
                  list("test"=NULL,"id"=NULL))


testlist %>% map(function(x) modify_at(x, "id", function(y) y="one"))

library(shiny)

choices <- list(a = 1, B = list(B = 2), c = list(3), d = list(fi = "fo", fum = "5"))

choices <- list(
  Spatial = list("Local", "Regional", "national", "global"),
  Temporal = list("daily", "weekly", "monthly", "annual", "decadal", "forecast")
)

ui <- bootstrapPage(
  selectizeInput("x1", "x", choices = choices, 
                 # options = list(placeholder = "test"),
                 # selected = NULL,
                 multiple = TRUE,
                 options = list(placeholder = '')
                 ),
  selectizeInput("x2", "x", multiple = TRUE, choices = NULL, selected = NULL)
)

server <- function(input, output, session) {
  updateSelectizeInput(session, "x2", choices = choices, server = TRUE)
}

shinyApp(ui, server)



metaquest_fields <- read_json("metaquest_fields_tester_list.json")
metaquest_fields[[2]]


metaquest_fields$panels[[1]]$sections[[2]]$fields[[4]]$fields %>% map(function(x) modify_at(x, "id", function(y) y=paste0(y, "test")))

test <- 
  metaquest_fields$panels[[1]]$sections[[2]]$fields[[4]]$fields %>% map(function(x) modify_at(x, "id", function(y) y=paste0(y, "test")))

testnames <- c("proj_keywords-1-", "proj_keywords-1-", "proj_keywords-12-", "proj_fail-1-", "test_proj_keywords-123-")
testid <- "proj_keywords"

testnames %>%
  # str_subset(paste0("^", id, "-\\d+-")) %>%
  str_extract(paste0("(?<=\\b", testid, "-)\\d+(?=-)")) %>%
  discard(is.na) %>%
  unique %>% as.numeric() %>% print


testx <- 1 %>% print




metaquest_fields$panels[[1]]$sections[[2]]$fields[[4]]$fields %>% map(pluck("id")) 
metaquest_fields$panels[[1]]$sections[[2]]$fields[[4]]$fields %>% map(pluck("id")) %>% map(str_glue("{x}Test"))
tester<-metaquest_fields$panels[[1]]$sections[[2]]$fields[[4]]$fields %>% map(pluck("id")) %>%
  {str_glue("{.}Test")}
for(test in tester){
  print(test)
}

x <- c("Crop provisioning", "Grazed biomass provisioning", "Timber provisioning", "Non-timber forest products and other biomass provisioning (e.g. maple syrup tapping, caribou hunting)", "Fish and other aquatic products provisioning", "Water supply (drinking, irrigation)", "Carbon storage (aboveground, belowground)", "Carbon sequestration", "Local (micro) climate regulation", "Air filtration services", "Soil erosion control services", "Water purification services", "Flood prevention/water retention services", "Riverine flood mitigation services (in-stream)", "Coastal flood protection", "Pollination services", "Pest control services", "Habitat maintenance services", "Recreation-related services", "Cultural aspects") %>% tolower()

c("crop provisioning", "grazed biomass provisioning", "timber provisioning", "non-timber forest products and other biomass provisioning (e.g. maple syrup tapping, caribou hunting)", "fish and other aquatic products provisioning", "water supply (drinking, irrigation)", "carbon storage (aboveground, belowground)", "carbon sequestration", "local (micro) climate regulation", "air filtration services", "soil erosion control services", "water purification services", "flood prevention/water retention services", "riverine flood mitigation services (in-stream)", "coastal flood protection", "pollination services", "pest control services", "habitat maintenance services", "recreation-related services", "cultural aspects")
c("Equal",
  "Conceptualization",
  "Data curation",
  "Formal analysis",
  "Funding acquisition",
  "Investigation",
  "Methodology",
  "Project administration",
  "Resources",
  "Software",
  "Supervision",
  "Validation",
  "Visualization",
  "Writing – original draft",
  "Writing – review & editing")


x <- jsonlite::read_json("/home/jclark/Downloads/unnamedPreparer_metaquest_2024-02-05_21-49-20.json")

x1 <- x
x1$test <- "whatever"

x1[!(x1 %>% names)]

keep(x1, str_detect(names(x1), names(x))) 
str_detect(names(x1), names(x))

x1[!(x1 %>% names)]

x1[!names(x1) %in% names(x)]


