
# choices -----------------------------------------------------------------

choices_affiliation <- c("-",
                         "Landscape 1",
                         "Landscape 2",
                         "Landscape 3",
                         "Landscape 4",
                         "Landscape 5",
                         "Lanscape 6",
                         "Theme 1",
                         "Theme 2",
                         "Theme 3",
                         "Synthesis",
                         "Other")


# questions ---------------------------------------------------------------


metaquests <- tibble::tribble(
                 ~id,        ~type,        ~label,    ~panel,   ~section,              ~choices,                                                  ~info,
         "prep_name",     "textIn",        "Name", "general", "preparer",                    NA,                                            NA,
  "prep_affiliation",   "selectIn", "Affiliation", "general", "preparer",   choices_affiliation,                    "Your research group within ResNet",
        "prep_email",     "textIn",       "Email", "general", "preparer",                    NA,               "Email you use for ResNet communication",
         "prep_date",     "dateIn",        "Date", "general", "preparer",                    NA,                                         "Today's date",
        "proj_title",     "textIn",       "Title", "general",  "project",                    NA, "A concise, descriptive title for the overall project",
     "proj_abstract", "textareaIn",    "Abstract", "general",  "project",                    NA,               "A brief abstract desribing the project",
     "proj_location",     "textIn",       "Location", "general",  "project",                    NA, "A concise, descriptive title for the overall project",
     "proj_start",     "dateIn",       "Start Date", "general",  "project",                    NA, "The date work began on this project",
      "proj_end",     "dateIn",       "End Date", "general",  "project",                    NA, "The date the project was completed. Leave blank if the project is ongoing."
  
  
  )

proj_contrib_row <- tibble::tribble(
  ~id,    ~type,        ~label, ~value,
  "name", "textIn",        "Name", "test1",
  "institution", "textIn", "Institution", "test2",
  "email", "textIn",       "Email", "test3"
)


sensitive_panel <- list()

exception_section <-
  div(checkboxGroupInput(
    "excep_type",
    label = "Exception Types",
    choices = c(
      "Traditional Knowledge",
      "Human Subjects",
      "Sensitive Data",
      "Existing Restrictions",
      "Other..."
    )
  ),
  textAreaInput("excep_descrip", label = "Description")
  )

# sections ----------------------------------------------------------------



# panels ------------------------------------------------------------------


