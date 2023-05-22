plot_logger <-
  function(daily_data, c_site, pcp_data, tmp_data) {
    
    daily_data$name <- paste(daily_data$var, daily_data$depth, "mm")
    
    data_smc <-
      daily_data %>% filter(site == c_site) %>% filter(var == "smc")
    data_temp <-
      daily_data %>% filter(site == c_site) %>% filter(var == "temp")
    
    temp_pal <-  wes_palette("BottleRocket2", 7, type = c("continuous"))
    smc_pal <-  wes_palette("FantasticFox1", 6, type = c("continuous"))
    
    # adding rain
    temp_pal <- c("red", "blue", temp_pal)
    smc_pal <- c("darkblue", smc_pal)
    
    pcp_data$name = "precipitation"
    
    tmp_data_min <- tmp_data[c(1,3)]
    tmp_data_max <- tmp_data[1:2]
    
    
    tmp_data_min$name = "air temp. min."
    tmp_data_max$name = "air temp. max."
    
    base_plot_tmp <- plot_ly(
      data = tmp_data_min %>% ungroup(),
      x = ~ date,
      y = ~ tmp_min,
      color =  ~name,
      colors = temp_pal,
      name = ~name,
      type = "scatter",
      mode = "lines",
      visible = "legendonly"
    )
    
    base_plot_tmp<-base_plot_tmp %>% add_trace(
      data = tmp_data_max %>% ungroup(),
      x = ~ date,
      y = ~ tmp_max,
      color =  ~name,
      colors = temp_pal,
      name = ~name,
      type = "scatter",
      mode = "lines",
      visible = "legendonly"
    )
    
    fig1 <- base_plot_tmp %>% add_trace(
        data = data_temp,
        x = ~ date,
        y = ~ value,
        color = ~ name,
        type = "scatter",
        mode = "lines",
        colors = temp_pal,
        line = list(shape = "linear"),
        visible = "TRUE"
      )
    
    fig1 <- fig1 %>% layout(
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = '#f5f5f5',
      title = paste("Logger", c_site),
      yaxis = list(title = 'Temp. (C)')
    )
    
    
    base_plot_smc <- plot_ly(
      data = pcp_data %>% ungroup(),
      x = ~ date,
      y = ~ pcp,
      color =  ~ name,
      colors = smc_pal,
      name = ~ name,
      type = "bar",
      visible = "legendonly"
    )
    
    
    fig2 <- base_plot_smc %>%
      add_trace(
        data = data_smc,
        x = ~ date,
        y = ~ value,
        color = ~ name,
        type = "scatter",
        mode = "lines",
        colors = smc_pal,
        line = list(shape = "linear"),
        visible = "TRUE"
      )
    
    fig2 <- fig2 %>%  layout(
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = '#f5f5f5',
      title = paste("Logger", c_site),
      yaxis = list(title = 'SMC (%)')
    )
    
    plot <-
      plotly::subplot(fig1,
                      fig2,
                      nrows = 2,
                      titleY = T,
                      which_layout = 1)
    return(plot)
  }

