## does athlete_id do anything?
pre_process_act <- function(df_act_raw){


  df_act_raw %>%
    select(id, name, type, workout_type, distance, moving_time, total_elevation_gain, start_date_local) %>%
    mutate(
           id = as.character(id),
           start_date_local = ymd_hms(start_date_local),
           year = year(start_date_local),
           month = month(start_date_local, label = TRUE),
           wkday = fct_relevel(wday(start_date_local, label=TRUE),
                            c("Sun","Sat","Fri","Thu","Wed","Tue","Mon")),
           day = day(start_date_local),
           wk = format(start_date_local, "%W"),
           hour = hour(lubridate::round_date(ymd_hms(start_date_local),unit="hour")),
           distance_k = distance / 1000,
           time_min = moving_time / 60,
           min_per_km = time_min / distance_k,
           race_or_not = ifelse(workout_type==1,"race","not a race") ) %>%
    arrange(., start_date_local) %>%
    # group_by(year) %>%
    # mutate(run_order = order(start_date_local),
    #        cumulative_distance = cumsum(distance_k),
    #        cumulative_time = cumsum(time_min)) %>%
    # ungroup() %>%
    split(., f = as.factor(.$type)) %>%
    purrr::map(., ~ (.x %>% select(-type)))

}




# select(distance, moving_time, total_elevation_gain, start_date_local)

# mutate(year = year(run_ymd),
#        month = month(run_ymd, label = TRUE),
#        wkday = fct_relevel(wday(run_ymd, label=TRUE),
#                            c("Sun","Sat","Fri","Thu","Wed","Tue","Mon")),
#        day = day(run_ymd),
#        wk = format(run_ymd, "%W"))


# run_year = lubridate::year(start_date_local),
# run_day = lubridate::wday(start_date_local),
# run_hour = hour(lubridate::round_date(ymd_hms(start_date_local),unit="hour")),
# run_ymd = lubridate::date(start_date_local),
# distance_k = distance / 1000,
# time_min = moving_time / 60,
# min_per_km = time_min / distance_k,

# arrange(., start_date_local) %>%
#   group_by(run_year) %>%
#   mutate(run_order = order(start_date_local),
#          cumulative_distance = cumsum(distance_k),
#          cumulative_time = cumsum(time_min)) %>%
#   ungroup()
#
