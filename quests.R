
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
     "proj_abstract", "textareaIn",    "Abstract", "general",  "project",                    NA,               "A brief abstract desribing the project"
  )


# sections ----------------------------------------------------------------



# panels ------------------------------------------------------------------


