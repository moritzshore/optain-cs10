test_swat_mod <- function() {

  SWAT_exe = "Rev_60_5_6_64rel.exe"  
  # delete contents of the run folder
  unlink("model_data/cs10_setup/run_swat", recursive = T) 
  
  dir.create("model_data/cs10_setup/run_swat/")
  
  sta_files <- list.files("model_data/cs10_setup/optain-cs10/",
                          pattern = "sta_", full.names = T)
  
  cli_files <- list.files("model_data/cs10_setup/optain-cs10/", 
                          pattern = ".cli", full.names = T)
  

  
  input_files_base <- list.files("model_data/cs10_setup/swat_input/", 
                            full.names = T)
  
  path_to_swat <- paste0("model_data/cs10_setup/", SWAT_exe)
  
  
  source_files <- c(sta_files, cli_files,path_to_swat, input_files_base)
  
  
  status <-
    file.copy(from = source_files,
              to = "model_data/cs10_setup/run_swat/",
              overwrite = T)
  
  
  
  if(any(status == FALSE)){
    warning("Some files were not copied:")
    print(source_files[which(status==FALSE)])
  }
  
  print(
    paste(
      length(which(status)),
      "of",
      length(source_files),
      "files were copied into the run folder, amounting to a size of ",
      round(sum(file.info(source_files)$size * 1e-6), 2),
      "megabytes"
    )
  )
  
  print("overwriting base files with modified files")
  
  input_files_mod <- list.files("model_data/cs10_setup/swat_input_mod/", 
                                full.names = T)
  status_mod <-
    file.copy(from = input_files_mod,
              to = "model_data/cs10_setup/run_swat/",
              overwrite = T)
  
  
  
  if(any(status_mod == FALSE)){
    warning("Some files were not copied:")
    print(input_files_mod[which(status_mod==FALSE)])
  }
  
  print(
    paste(
      length(which(status_mod)),
      "of",
      length(input_files_mod),
      "modified files were copied into the run folder, amounting to a size of ",
      round(sum(file.info(input_files_mod)$size * 1e-6), 2),
      "megabytes"
    )
  )
  
  print("updating time of simulation")
  
  # update time sim
  time_sim <- readLines("model_data/cs10_setup/run_swat/time.sim")
  time_sim[3] <- "       0      2016         0      2016         0  "
  writeLines(text = time_sim, con = "model_data/cs10_setup/run_swat/time.sim")
  

  print("running model for 1 year")
  
  
  # run model
  msg <- processx::run(command = SWAT_exe,
                       wd = "model_data/cs10_setup/run_swat/", spinner = T)
  
  simout <- readLines("model_data/cs10_setup/run_swat/simulation.out")
  
  return(simout)
  
}
