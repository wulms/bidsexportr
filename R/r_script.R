# main function ----

#' Wrapper function
#'
#' @return
#' @export
#'
#' @examples
start_export <- function(){
  create_presets()
  
  df_metadata <<- create_bids_metadata()
  df_bids_data <<- create_bids_df()
  
  
  filter_dialog()
  copy_files(input = df_bids_data_filter$files_input,
             output = df_bids_data_filter$files_output)
  
  fs::file_copy(df_metadata$input, df_metadata$output)
  
  create_participants_tsv()
  
}


# presets ----

#' Selects the input and output folder for file export
#'
#' @return
#' @export
#'
#' @examples
create_presets <- function(){
  bids_path <<- shinyDirectoryInput::choose.dir(caption = "Choose input directory") %>%
    normalizePath(winslash = "/")
  
  print(paste("Your input folder: ", bids_path))
  
  check_bids_path()
  output_path <<- shinyDirectoryInput::choose.dir(caption = "Choose output directory") %>%
    normalizePath(winslash = "/")
  cat("\n\n")
  print(paste("Your output folder: ", output_path))
}

## select form ----


#' Privides the CSV filter functionality. 
#'
#' @return
#' @export
#'
#' @examples
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
        dplyr::rename(subject_ids_short = 1,
                      session_ids_short = 2) %>%
        dplyr::mutate(subject_ids_short = as.character(subject_ids_short),
                      session_ids_short = as.character(session_ids_short))
    } else if (ncol(filter_df) == 1){
      print("One column identified.")
      filter_df_out <- filter_df %>%
        dplyr::rename(subject_ids_short = 1) %>%
        dplyr::mutate(subject_ids_short = as.character(subject_ids_short))
    } else {
      print("CSV does not match.")
      stop()
    }
    print(filter_df_out)
    
    df_bids_data_filter <<- filter_df_out %>% 
      dplyr::left_join(df_bids_data) %>%
      dplyr::filter(!is.na(files_input))
    
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

#' Starts the "filter dialog" for sequence, session and regular expressions in the BIDS dataset
#'
#' @return
#' @export
#'
#' @examples
filter_dialog <- function(){
  
  filter_file_dialog()
  
  svDialogs::dlg_message(message = "To select multiple options hold the 'Strg/Ctrl key' during selection.",
                         type = "ok")
  
  
  
  if(unique(df_bids_data$session_ids_short) %>% length() > 1 ){
    
    filter_sessions <<- svDialogs::dlg_list(title = "SESSION:",
                                            choices = unique(df_bids_data$session_ids_short),
                                            multiple = TRUE)$res
    
    print(paste("You selected:", filter_sessions))
    
    if(length(filter_sessions) == 0){
      print("No sessions identified. No valid BIDS set.")
      stop()
    } else {
      df_bids_data_filter <<- df_bids_data_filter %>%
        dplyr::filter(session_ids_short %in% filter_sessions)
    }
  }
  
  
  
  
  
  filter_sequences <<- svDialogs::dlg_list(title = "SEQUENCE:",
                                           choices = unique(df_bids_data$sequence_ids),
                                           multiple = TRUE)$res
  
  print(paste("You selected:", filter_sequences))
  
  
  if(length(filter_sequences) == 0){
    print("No sequences identified. No valid BIDS set.")
    stop()
  } else {
    df_bids_data_filter <<- df_bids_data_filter %>%
      dplyr::filter(sequence_ids %in% filter_sequences)
  }
  
  filter_cohort <<- svDialogs::dlg_input(message = "Enter a regular expression, e.g. '^1' to select only ids beginning with 1")$res %>% 
    stringr::regex()
  
  print(paste("You selected:", filter_cohort))
  
  
  if(length(filter_cohort) == 0){
    print("No regular expression selected. Keeping all data.")
  } else {
    df_bids_data_filter <<- df_bids_data_filter %>%
      dplyr::filter(stringr::str_detect(subject_ids_short, filter_cohort))
  }
  
  
  svDialogs::dlg_message(message = "Do you want only complete observations per participant, containing ALL selected sequence types? Then select 'Yes' in the next window. 'No' selects also observations with missing data.",
                         type = "ok")
  
  filter_complete <<- svDialogs::dlg_list(title = "Only complete?",
                                           choices = c("Yes", "No"),
                                           multiple = FALSE)$res
  
  print(paste("You selected:", filter_complete))
  
  if(filter_complete == "Yes"){
    print("Filtering out incomplete sequences")
    df_bids_data_filter <<- df_bids_data_filter %>%
      dplyr::group_by(subject_ids_short, session_ids_short) %>%
      dplyr::mutate(sequence_count = dplyr::n()/2) %>%
      dplyr::filter(sequence_count == length(filter_sequences)) %>%
      dplyr::ungroup()
  } else {
    print("Nothing changed.")
  }
  
  
  print(df_bids_data_filter)
  
  print("Data overview:")
  
  df_bids_data_filter %>%
    dplyr::select(-subject_ids_short) %>%
    dplyr::filter(type_ids == ".nii.gz" | type_ids == ".nii") %>%
    dplyr::count(session_ids_short, sequence_ids) %>%
    tidyr::pivot_wider(names_prefix = "session ",
                       names_from = session_ids_short,
                       values_from = n) %>%
    print()
  
  if(svDialogs::dlg_list(title = "Start copy process?",
                      choices = c("Yes", "No"),
                      multiple = FALSE)$res == "No"){
    stop("Process stopped by user.")
  }
}

#' Checks the filter criterias
#'
#' @param df 
#' @param session 
#' @param sequences 
#' @param cohort 
#'
#' @return
#' @export
#'
#' @examples
test_filter <- function(df, 
                        session = filter_sessions, 
                        sequences = filter_sequences,
                        cohort = filter_cohort){
  df %>% 
    dplyr::filter(sequence_ids %in% sequences) %>%
    dplyr::filter(session_ids_short %in% session) %>%
    dplyr::filter(stringr::str_detect(subject_ids_short, cohort))
}

#' Creates an adapted participants.tsv in the output path
#'
#' @param output_path 
#' @param df 
#' @param session 
#' @param sequences 
#' @param cohort 
#'
#' @return
#' @export
#'
#' @examples
create_participants_tsv <- function(path = output_path,
                                    df = df_bids_data_filter,
                                   session = filter_sessions, 
                                   sequences = filter_sequences,
                                   cohort = filter_cohort){
  tsv_path <- paste0(path, "/participants.tsv")
  print(tsv_path)
  
  if(!file.exists(tsv_path)){
    df_bids_data_filter %>% 
      dplyr::select(subject_ids, session_ids) %>%
      dplyr::rename(participant_id = subject_ids,
                    session = session_ids) %>%
      dplyr::distinct() %>%
      readr::write_tsv(., file = tsv_path)
    
  }
    
  
}

#' Creates mock data for dfs
#'
#' @return
#' @export
#'
#' @examples
create_mock_data <- function(){
  subject_df_one <- tibble(idbidirect = c(10004, 10005, 10008))
  readr::write_csv(subject_df_one, file = "df_one.csv")
  
  subject_df_two <- tibble(idbidirect = c(10004, 10005, 10008),
                           session = c("0", "0", "0"))
  readr::write_csv(subject_df_two, file = "df_two.csv")
}

## test functions ----

#' Tests if the BIDS folder contains a "participants.tsv" file
#'
#' @return
#' @export
#'
#' @examples
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

#' Creates the BIDS metadata paths
#'
#' @return
#' @export
#'
#' @examples
create_bids_metadata <- function(path = bids_path){
  df_bids_metadata <- tibble::tibble(input = list.files(path, pattern = "CHANGES|README|json|tsv",
                                                recursive = FALSE,
                                                full.names = TRUE,
                                                include.dirs = TRUE)) %>%
    dplyr::mutate(output = stringr::str_replace(input, path, output_path))
  
  return(df_bids_metadata)
}


# all files

#' Reads the folders
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
list_BIDS_files <- function(path = bids_path){
  print("Reading folder. This will take some time.")
  files <<- list.files(path, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  print(head(files, n = 50))
}

#' Creates a BIDS dataframe with all relevant information
#'
#' @return
#' @export
#'
#' @examples
create_bids_df <- function(path = bids_path, output = output_path){
  
  list_BIDS_files()
  
  #files <- head(files, n = 100)
  
  print(head(files, n = 50))
  
  df_files <- tibble::tibble(files_input = files) %>%
    dplyr::mutate(files_output = stringr::str_replace(files_input, path, output)) %>%
    dplyr::mutate(files_short = stringr::str_extract(files, "sub-[:alnum:]+_ses-[:alnum:]+_.*$|sub-[:alnum:]+_.*$")) %>%
    dplyr::mutate(subject_ids = stringr::str_extract(files_short, "sub-[:alnum:]+(?=_.*$)"),
                  subject_ids_short = stringr::str_remove(subject_ids, "sub-"),
                  
                  session_ids = stringr::str_remove(files_short, subject_ids) %>%
                    stringr::str_extract("ses-[:alnum:]+_.$*") %>% 
                    stringr::str_extract("ses-[:alnum:]+(?=.*$)"),
                  session_ids_short = stringr::str_remove(session_ids, "ses-"),
                  
                  type_ids = stringr::str_extract(files_short, "\\.(nii\\.gz|nii|json|bval|bvec)"),
                  
                  sequence_ids = stringr::str_extract(files_short, "(sub-[:alnum:]+_ses-[:alnum:]+_|sub-[:alnum:]+_).*$") %>%
                    stringr::str_remove(paste0(subject_ids, "_")) %>%
                    stringr::str_remove(paste0(session_ids, "_")) %>%
                    stringr::str_remove(type_ids)
    ) %>%
    dplyr::filter(!is.na(files_short)) %>%
    dplyr::filter(!is.na(sequence_ids)) %>%
    dplyr::filter(!stringr::str_detect(files_input, "/derivatives/"))
  
  df_files %>%
    dplyr::count(session_ids_short, sequence_ids)  %>%
    tidyr::pivot_wider(names_prefix = "session ",
                       names_from = session_ids_short,
                       values_from = n) %>% print()
  
  
  return(df_files)
}



# functions copy files ----

#' Copies non existant files from one location to another
#'
#' @return
#' @export
#'
#' @examples
copy_files <- function(input, output){
  for (i in 1:length(input)) {
    print(paste("Copied file: ", output[i]))

    path_to_folder(output[i])
    fs::file_copy(path = input[i], new_path = output[i])
  }

}

#' Creates folder paths from file paths
#'
#' @return
#' @export
#'
#' @examples
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
