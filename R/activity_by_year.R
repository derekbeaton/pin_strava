activity_by_year <- function(activity_df, year_to_pull = "2021"){

  activity_df %>%
    filter(lubridate::year(start_date) == year_to_pull)

}
