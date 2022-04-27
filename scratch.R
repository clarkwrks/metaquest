test_list <- list(
  prep_name = "Test User",
  prep_affiliation = "Landscape 1",
  prep_email = "test@us.er",
  prep_date = "2022-04-12",
  proj_title = "Example Project",
  proj_abstract = "Wonderful science"
)

tmp <- tempfile()
write_json(test_list, tmp)

read_json(tmp)

read_json("/home/jclark/Downloads/test.json")
