highchart_legend <-
  function(legend_names = NA,
           legend_colours = NA,
           shape = "square",
           radius = 10) {
    
    ## Make empty chart
    chart <- highchart() %>%
      hc_legend(
        align = "center",
        verticalAlign = "middle",
        floating = TRUE,
        itemMarginBottom = 10,
        enabled = TRUE,
        navigation = list(
          enabled = FALSE
        )
      ) %>%
      hc_xAxis(gridLineWidth = 0,
               minorGridLineWidth = 0,
               tickWidth = 0,
               lineWidth = 0,
               labels = list(
                 enabled = FALSE
               )) %>%
      hc_title(text = "") %>%
      hc_chart(height = 150) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_credits(enabled = FALSE)
    
    ## Append series
    invisible(lapply(1:length(legend_names), function(colNumber) {
      chart <<-
        hc_add_series(
          hc = chart,
          name = legend_names[colNumber],
          marker = list(symbol = shape, radius = radius),
          color = legend_colours[colNumber],
          events = list(legendItemClick = FALSE) 
        )
    }))
    ## Return
    chart
  }