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

for(i in list_inputs){
  if(x )
}