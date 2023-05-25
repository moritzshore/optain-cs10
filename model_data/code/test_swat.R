sta_files <- list.files("model_data/cs10_setup/optain-cs10/",
                        pattern = "sta_", full.names = T)

cli_files <- list.files("model_data/cs10_setup/optain-cs10/", 
                        pattern = ".cli", full.names = T)

input_files <- list.files("model_data/cs10_setup/swat_input/", 
                          full.names = T)

source_files <- c(sta_files, cli_files, input_files)


status <- file.copy(from = source_files, to = "model_data/cs10_setup/run_swat/")

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