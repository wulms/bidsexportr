# main function ----

start_export <- function(){
  create_presets()

  df_metadata <<- create_bids_metadata()
  df_bids_data <<- create_bids_df()


  filter_dialog()
  copy_files(input = df_bids_data_filter$files_input,
             output = df_bids_data_filter$files_output)

  fs::file_copy(df_metadata$input, df_metadata$output)
}


# presets ----
create_presets <- function(){
  bids_path <<- shinyDirectoryInput::choose.dir(caption = "Choose input directory") %>%
    normalizePath(winslash = "/")
  check_bids_path()
  output_path <<- shinyDirectoryInput::choose.dir(caption = "Choose output directory") %>%
    normalizePath(winslash = "/")
}

## select form ----

filter_file_dialog <- function(){

  # choose file (yes/no)
  filter_file_option <- svDialogs::dlgMessage(
    message = "Do you want to provide a CSV file (one column = IDs, two column = IDs, sessions)?",
    type = "yesno")$res

  if(filter_file_option == "yes"){

    svDialogs::dlg_message(message = "When you use a CSV - take care, that IDs match! \n\n First column = SUBJECT-IDs. \n\n Optional second column = SESSION-ID",
                           type = "ok")

    filter_file <<- svDialogs::dlg_open(
      title = "Select your CSV with IDs or IDs and session headers")$res

    filter_df <- readr::read_csv(filter_file)


    if(ncol(filter_df) == 2){
      print("Two columns identified.")
      filter_df_out <- filter_df %>%
        rename(subject_ids_short = 1,
               session_ids_short = 2) %>%
        mutate(subject_ids_short = as.character(subject_ids_short),
               session_ids_short = as.character(session_ids_short))
    } else if (ncol(filter_df) == 1){
      print("One column identified.")
      filter_df_out <- filter_df %>%
      rename(subject_ids_short = 1) %>%
        mutate(subject_ids_short = as.character(subject_ids_short))
    } else {
      print("CSV does not match.")
      stop()
    }
    print(filter_df_out)

    df_bids_data_filter <<- filter_df_out %>% left_join(df_bids_data) %>%
      filter(!is.na(files_input))

    if(nrow(df_bids_data_filter) == 0){
      print("No matches found.")
      stop()
    }

    print(df_bids_data_filter)

  } else {
    print("No file selected.")

    df_bids_data_filter <<- df_bids_data
  }
}

filter_dialog <- function(){

  filter_file_dialog()

  svDialogs::dlg_message(message = "To select multiple options hold the 'Strg/Ctrl key' during selection.",
                           type = "ok")
    filter_sessions <<- svDialogs::dlg_list(title = "SESSION:",
                                            choices = unique(df_bids_data$session_ids_short),
                                            multiple = TRUE)$res

    if(length(filter_sessions) == 0){
      print("No sessions selected.")
      stop()
    } else {
      df_bids_data_filter <<- df_bids_data_filter %>%
        filter(session_ids_short %in% filter_sessions)
    }

    filter_sequences <<- svDialogs::dlg_list(title = "SEQUENCE:",
                                             choices = unique(df_bids_data$sequence_ids),
                                             multiple = TRUE)$res

    if(length(filter_sequences) == 0){
      print("No sequences selected.")
      stop()
    } else {
      df_bids_data_filter <<- df_bids_data_filter %>%
        filter(sequence_ids %in% filter_sequences)
    }

    filter_cohort <<- svDialogs::dlg_input(message = "Enter a regular expression, e.g. '^1' to select only ids beginning with 1")$res %>% regex()

    if(length(filter_cohort) == 0){
      print("No regular expression selected. Keeping all data")
    } else {
      df_bids_data_filter <<- df_bids_data_filter %>%
        filter(str_detect(subject_ids_short, filter_cohort))
    }

    print(df_bids_data_filter)

    df_bids_data_filter %>%
      filter(type_ids == ".nii.gz") %>%
      count(session_ids_short, sequence_ids) %>%
      pivot_wider(names_prefix = "session ",
                  names_from = session_ids_short,
                  values_from = n)
}





create_mock_data <- function(){
  subject_df_one <- tibble(idbidirect = c(10004, 10005, 10008))
  readr::write_csv(subject_df_one, file = "df_one.csv")

  subject_df_two <- tibble(idbidirect = c(10004, 10005, 10008),
                           session = c("0", "0", "0"))
  readr::write_csv(subject_df_two, file = "df_two.csv")
}

## test functions ----
check_bids_path <- function(path = bids_path){
  if (list.files(path,
                 pattern = "participants\\.tsv") %>%
      length() == 1) {
    svDialogs::dlg_message("BIDS folder accepted: Participants.tsv found.")
  } else {
    svDialogs::dlg_message("Stopping execution. No participants.tsv found. No valid BIDS folder.")
    stop()
  }

}


## functions data ----
create_bids_metadata <- function(path = bids_path){
  df_bids_metadata <- tibble(input = list.files(path, pattern = "CHANGES|README|json",
                                                   recursive = FALSE,
                                                   full.names = TRUE,
                                                   include.dirs = TRUE)) %>%
    mutate(output = str_replace(input, path, output_path))

  return(df_bids_metadata)
}


# all files

create_bids_df <- function(path = bids_path, output = output_path){

  print("Reading folder. This will take some time.")
  files <- list.files(path, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  files_test <- head(files, n = 100)

  df_files <- tibble(files_input = files_test) %>%
    mutate(files_output = str_replace(files_input, path, output)) %>%
    mutate(files_short = str_extract(files_test, "sub-[:alnum:]+_ses-[:alnum:]+_.*$"),
           subject_ids = str_extract(files_short, "sub-[:alnum:]+(?=_ses-[:alnum:]+_.*$)"),
           subject_ids_short = str_remove(subject_ids, "sub-"),
           session_ids = str_remove(files_short, subject_ids) %>%
             str_extract("ses-[:alnum:]+_.$*") %>% str_extract("ses-[:alnum:]+(?=.*$)"),
           session_ids_short = str_remove(session_ids, "ses-"),
           type_ids = str_extract(files_short, "\\.(nii\\.gz|json|bval|bvec)"),
           sequence_ids = str_extract(files_short, "sub-[:alnum:]+_ses-[:alnum:]+_.*$") %>%
             str_remove(paste0(subject_ids, "_", session_ids, "_")) %>%
             str_remove(type_ids)
    ) %>%
    filter(!is.na(files_short))

  df_files %>%
    count(session_ids_short, sequence_ids)  %>%
    pivot_wider(names_prefix = "session ",
                names_from = session_ids_short,
                values_from = n)


  return(df_files)
}



# functions copy files ----
copy_files <- function(input, output){
  for (i in 1:length(input)) {
    print(paste("Copied file: ", output[i]))

    path_to_folder(output[i])
    fs::file_copy(path = input[i], new_path = output[i])
  }

}

path_to_folder <- function(list_of_files) {
  paths_folder <- sub("[/][^/]+$", "", list_of_files)
  paths_folder <- unique(paths_folder)

  paths_folder <- paths_folder[!dir.exists(paths_folder)]
  # print(head(paths_folder))
  lapply(paths_folder,
         dir.create,
         recursive = TRUE,
         showWarnings = FALSE)
}
