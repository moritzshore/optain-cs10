plot_api <- function(api_vals = NULL, obs = NULL, title = "blank") {
  
  yrange <- c(obs$value, obs$pcp, api_vals$val)
  
  ylimits <- c(min(yrange, na.rm = T), max(yrange, na.rm = T)+10)
  
  par(mar = c(5, 4, 4, 4) + 0.3)  # Additional space for second y-axis
  
  plot(
    obs$date,
    y = obs$value,
    col = "black",
    type = "l",
    ylab = "Total water content (cm)",
    axes = F,
    xlab = "Date",
    ylim = ylimits
  )
 
  
  lines(obs$date, api_vals$val, col = "red")
  
  par(new = TRUE)
  
  plot(
    obs$date,
    y = obs$pcp,
    col = "blue",
    type = "h",
    main = title,
    ylab = "",
    xlab = "Date",
    ylim = ylimits
  )
  
  axis(side = 4, at = pretty(range(obs$pcp)))
  mtext("mm", side = 4, line = 3)  
  
  legend(x = "topright", legend=c("API", "Measured", "Precipitation"),
         col=c("red", "black", "blue"), lty=1:2, cex=0.8)
  
}
