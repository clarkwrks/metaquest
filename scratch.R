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