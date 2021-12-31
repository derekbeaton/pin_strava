filter_by_distance <- function(activity_df, min_meters = 0, max_meters = 5000){

  activity_df %>%
    filter(distance >= min_meters & distance < max_meters + .Machine$double.eps)

}
