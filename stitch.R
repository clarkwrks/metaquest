# Walk through metaquest_fields like fields.R, but output html elements instead of Shiny inputs
# Match field ID's to user generated JSON to pull values
# Need to capture deprecated/unmatched user input

metaquest_fields <- read_json("metaquest_fields.json")
input_fields <- read_json("test_data/CatherineDestrempes_metaquest_2024-01-30_15-41-00 (6).json")


# parse input -------------------------------------------------------------


buildPanel <- function(panel_json, input_json){
  div(
    h2(panel_json$title),
    panel_json$sections %>% map(buildSection, input_json)
  )
}

buildSection <- function(section_json, input_json){
  div(
    h3(section_json$heading),
    HTML(section_json$description),    
    section_json$fields %>% map(buildField, input_json)
  )
}

buildField <- function(field_json, input_json){
  
  field_input_id <- paste0(field_json$id, "-Input")
  
  if(field_json$type == "listInput") {
    div(
      h4(field_json$label),
      buildListField(field_json, input_json)
      )
  } else {
    
    
    buildFieldValue(
      field_label = field_json$label,
      field_value = input_json %>% pluck(field_input_id)
    )
  }
}

buildListField <- function(field_json, input_json){
  listfield_id <- field_json$id
  
  listfield_inputs <- input_json[input_json %>% names %>% str_starts(listfield_id)]
  listfield_rownumbers <- listfield_inputs %>% names %>% str_extract("\\d+") %>% 
    unique() %>% na.omit() %>% as.vector

  listfield_html <- listfield_rownumbers %>% map(buildListFieldRow, field_json, listfield_inputs, listfield_id)

  div(
  listfield_html
  )
  
}

buildListFieldRow <- function(rownumber, rowfield_json, input_json, listfield_id){
  rowfield_json <- rowfield_json$fields %>% 
    map(\(x) modify_at(x, "id", ~ paste(listfield_id, rownumber, ., "Input", sep="-")))
  
  div(
    rowfield_json %>% map(buildRowField, rownumber, input_json),
    hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
    br()
  )
}

buildRowField <- function(rowfield_json, rownumber, input_json){
  rowfield_id <- rowfield_json$id

  buildFieldValue(rowfield_json$label, input_json %>% pluck(rowfield_id))
  
}

buildFieldValue <- function(field_label, field_value){
  field_label <- field_label %>% str_trim() %>% paste0(": ")
  
  div(class="mq-field",
    span(class="mq-label", field_label),
    span(class="mq-value", field_value)
  )
}

# capture unmatched input -------------------------------------------------

buildUnmatched <- function(metaquest_json, input_json){
  metaquest_fields_id <- metaquest_json %>% 
    unlist %>% 
    .[str_ends(names(.), ".id")] %>% 
    paste0("-Input")
  
  form_input_match <- str_detect(input_json %>% names, 
                                 str_c(metaquest_fields_id, collapse = "|"))
  
  unmatched_input <- input_json[!form_input_match]

  unmatched_input %>% imap(\(field_value, field_label) buildFieldValue(field_label, field_value)) 
  
}

# stitch report -----------------------------------------------------------

stitchMetaquest <- function(input_json_path, metaquest_json, exportToPDF=FALSE){
  
  out_filename <- input_json_path %>% basename %>% tools::file_path_sans_ext()
  input_json <- read_json(input_json_path)
  
  report_html <- div(
    includeCSS("report.css"),
    metaquest_json$panels %>% map(buildPanel, input_json = input_json),
    div(h3("Unmatched Input"),
        buildUnmatched(metaquest_json, input_json)
    )
  )
  
  report_html_path <- paste0("temp/", out_filename, ".html")
  
  report_out <- report_html %>% htmltools::save_html(report_html_path)
  
  if(exportToPDF == TRUE){
    report_out <- chrome_print(report_html_path, paste0("temp/", out_filename, ".pdf"))
    print("HTML report created and converted to PDF")
  } else {
    report_out
    print("HTML report created")
  }
  
}

metaquest_fields <- read_json("metaquest_fields.json")
input_fields <- read_json("test_data/CatherineDestrempes_metaquest_2024-01-30_15-41-00 (6).json")

list.files("to_convert", full.names = TRUE) %>% map(stitchMetaquest, metaquest_fields, exportToPDF = TRUE)

# stitchMetaquest(metaquest_fields, "test_data/CatherineDestrempes_metaquest_2024-01-30_15-41-00 (6).json")
# stitchMetaquest(metaquest_fields, "test_data/AgnesVari_metaquest_2024-01-30_16-34-10.json", exportToPDF=TRUE)

