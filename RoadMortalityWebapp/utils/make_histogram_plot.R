# iii. Histogram of # records by year
make_histogram_plot = function(dat){
  renderPlot({

    num_unique_years = length(unique(dat$year))

    number_bins = ifelse(num_unique_years > 20, 20, num_unique_years)

    dat |>
      ggplot() +
      geom_histogram(aes(year),
                     fill = 'darkgreen',
                     bins = number_bins) +
      theme_minimal() +
      labs(y = 'Number of Records',
           x = 'Year',
           title = 'Road Mortality Events by Year') +
      scale_x_continuous(breaks = scales::breaks_pretty(n = ifelse(num_unique_years > 5, 5, num_unique_years))) +
      theme(
        title = element_text(size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
}
