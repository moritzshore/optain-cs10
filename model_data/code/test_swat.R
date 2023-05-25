test_swat <- function() {
  
  # delete contents of the run folder
  unlink("model_data/cs10_setup/run_swat", recursive = T) 
  
  dir.create("model_data/cs10_setup/run_swat/")
  
  sta_files <- list.files("model_data/cs10_setup/optain-cs10/",
                          pattern = "sta_", full.names = T)
  
  cli_files <- list.files("model_data/cs10_setup/optain-cs10/", 
                          pattern = ".cli", full.names = T)
  
  input_files <- list.files("model_data/cs10_setup/swat_input/", 
                            full.names = T)
  
  path_to_swat <- "model_data/cs10_setup/rev60.5.4_64rel.exe"
  
  
  source_files <- c(sta_files, cli_files,path_to_swat, input_files)
  
  
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
  
  # update time sim
  time_sim <- readLines("model_data/cs10_setup/run_swat/time.sim")
  time_sim[3] <- "       0      2010         0      2011         0  "
  writeLines(text = time_sim, con = "model_data/cs10_setup/run_swat/time.sim")
  
  # run model
  msg <- processx::run(command = "rev60.5.4_64rel.exe",
                wd = "model_data/cs10_setup/run_swat/", spinner = T)
  
  simout <- readLines("model_data/cs10_setup/run_swat/simulation.out")
  
  return(simout)

  }
