require(readr)
require(dplyr)
require(hydroGOF)
require(reticulate) # Required Python packages: "pandas", "numpy", "scipy"

source("model_data/code/plot_api.R")

calc_api <- function(logger_id) {
  logger_data <- read_csv("model_data/field_sites/loggers/clean/logger_data_daily_clean.csv", show_col_types=F)
  # we only need SMC for this part
  logger_data = logger_data %>% filter(var == "smc")
  site_attributes <- read_csv("model_data/field_sites/output/field_site_attr_df.csv", show_col_types=F)
  pcp_data <- read_csv("model_data/field_sites/loggers/clean/pcp_data_clean.csv", show_col_types = F)
  
  HSG <- site_attributes %>% filter(id == logger_id) %>% select(HYDGRP) %>% pull()
  
  hsg_data <- logger_data %>% filter(site == logger_id)
  
  a <- hsg_data %>% filter(depth == "300") %>% select(value)
  b <- hsg_data %>% filter(depth == "450") %>% select(value)
  c <- hsg_data %>% filter(depth == "600") %>% select(value)
  d <- hsg_data %>% filter(depth == "750") %>% select(value)
  e <- hsg_data %>% filter(depth == "900") %>% select(value)
  
  total_water_content <- 
    a * .35 + # from 0 to 35
    b * .15 + # from 35 to 50
    c * .15 + # from 55 to 65
    d * .15 + # from 65 to 80
    e * .20   # from 80 to 100
  
  # twc = total water content
  twc <- tibble(hsg_data %>% filter(depth == "300") %>% select(date), total_water_content)
  
  twc <- left_join(twc, pcp_data, by = "date")
  
  # drop na values
  twc <- twc[which(twc$value %>% is.na() == FALSE),]
  write_csv(twc, "model_data/field_sites/output/twc.csv")
  
  ## run py script
  
  source_python("model_data/code/calc_api.py", convert = T)
  
  
  optimized <- read_csv(
    "model_data/field_sites/output/api_optimized.csv",
    show_col_types = F,
    skip = 1,
    col_names = c("val", "date")
  )
  
  plot_title <- paste0("HSG-", HSG, " API optimized for logger",logger_id)
  plot_api(api_vals = optimized,
           obs = twc,
           title = plot_title)
  
  r_vector <- c(HSG, alpha_value)
  
  names(r_vector) <-c("HSG", "Alpha")
  return(r_vector)
}


