source_all_folders <- function( main_folder = getwd()
                                , subfolder_to_load = "R"
                                , pattern_for_choose_a_file_to_source = "\\.R$"

  ){

if(!is.null(subfolder_to_load )){  main_folder <- file.path(main_folder, subfolder_to_load)   }

R_files <- list.files(main_folder, recursive = T, all.files = T, full.names = T, pattern = pattern_for_choose_a_file_to_source)

# verif files to source :
if (length(R_files) == 0) {
  message("❌ No R files found ( ", main_folder, " )")
  return(invisible(NULL))
}

lapply(R_files, source)

}
