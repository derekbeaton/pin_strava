# non-target based

## bundle these together in a more clever way.
source("libraries.R")
walk(dir_ls("R"), source)

# athlete_id <- Sys.getenv("ATHLETE_ID")

## never used.
# act_col_types <- list(
#   moving = col_logical(), velocity_smooth = col_number(),
#   grade_smooth = col_number(), distance = col_number(),
#   altitude = col_number(), heartrate = col_integer(), time = col_integer(),
#   lat = col_number(), lng = col_number(), cadence = col_integer(),
#   watts = col_integer()
# )

my_app <- define_strava_app()

my_endpoint <- define_strava_endpoint()

my_sig <- define_strava_sig(my_endpoint, my_app)
# ,cue = tar_cue(mode = "always")

df_act_raw <- read_all_activities(my_sig)

## replace this with something slightly simpler?
  ## does athlete_id do anything?
  ## also this should be done over the separate lists OR I should also do this by type
    ## but by list and using that as what we iterate over is easier
df_act <- pre_process_act(df_act_raw, athlete_id)
df_act_test <- pre_process_act(df_act_raw)

## purrr'd up to get each type into separate fds within the list
  ## there's some analysis that can happen up here now.
# list_df_act <- df_act %>%
#   split(., f = as.factor(.$type)) %>%
#   purrr::map(., ~ (.x %>% select(-type)))


list_df_act %>%
  pluck("Run") %>%
  filter(distance >= 5000 & moving_time > 0) %>%
  select(distance, moving_time, total_elevation_gain, start_date_local) %>%
  mutate(run_year = lubridate::year(start_date_local),
         run_day = lubridate::wday(start_date_local),
         run_hour = hour(lubridate::round_date(ymd_hms(start_date_local),unit="hour")),
         run_ymd = lubridate::date(start_date_local),
         distance_k = distance / 1000,
         time_min = moving_time / 60,
         min_per_km = time_min / distance_k,
         run_class = factor(
             case_when(
              distance_k <= 10 ~ "5-10 km",
              distance_k > 10 & distance_k <= 15 ~ "10-15 km",
              distance_k > 15 & distance_k <= 18 ~ "15-18 km",
              distance_k > 18 & distance_k <= 21 ~ "18-21 km",
              distance_k > 21 & distance_k <= 24 ~ "21-24 km",
              distance_k > 24 ~ "24+ km",
            ),
            levels = c("5-10 km","10-15 km","15-18 km","18-21 km","21-24 km","24+ km")
          )
         ) %>%
  arrange(., start_date_local) %>%
  group_by(run_year) %>%
  mutate(run_order = order(start_date_local),
         cumulative_distance = cumsum(distance_k),
         cumulative_time = cumsum(time_min)) %>%
  ungroup -> over_5k_runs_huh



df_act_test %>%
  pluck("Run") %>%
  # filter(distance >= 5000 & moving_time > 0) %>%
  filter(moving_time > 0) %>%
  mutate(
    run_class =
      factor(
        case_when(
             distance_k <= 1 ~ "<= 1 km",
             distance_k > 1 & distance_k <= 5 ~ "1-5 km",
             distance_k > 5 & distance_k <= 10 ~ "5-10 km",
             distance_k > 10 & distance_k <= 15 ~ "10-15 km",
             distance_k > 15 & distance_k <= 18 ~ "15-18 km",
             distance_k > 18 & distance_k <= 21 ~ "18-21 km",
             distance_k > 21 & distance_k <= 24 ~ "21-24 km",
             distance_k > 24 ~ "24+ km",
            ),
           levels = c("<= 1 km","1-5 km","5-10 km","10-15 km","15-18 km","18-21 km","21-24 km","24+ km")
        )
  ) %>%
  group_by(year) %>%
  mutate(run_order = order(start_date_local),
         cumulative_distance = cumsum(distance_k),
         cumulative_time = cumsum(time_min)) %>%
  ungroup() -> over_5k_runs_test




over_5k_runs_test %>%
  ggplot(., aes( x = run_order, y = min_per_km, color = run_class)) +
  geom_path() +
  geom_point() +
  facet_grid(cols = vars(year),
             rows = vars(run_class))


over_5k_runs_test %>%
  ggplot(., aes(x = distance_k, y = time_min)) +
  geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  geom_point(aes(color = run_class))


over_5k_runs_test %>%
  ggplot(., aes(x = distance_k, y = time_min)) +
  facet_grid(cols = vars(year)) +
  geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  geom_point(aes(color = run_class))


# cumulative distance

over_5k_runs_test %>%
  ggplot(., aes(x = run_order, y = cumulative_distance, color = as.character(year))) +
  geom_line() +
  geom_point()

# cumulative time

over_5k_runs_test %>%
  ggplot(., aes(x = run_order, y = cumulative_time, color = as.character(year))) +
  geom_line() +
  geom_point()

# paces by year

# over_5k_runs %>%
#   ggplot(., aes(x = min_per_km, color = as.character(run_year), fill = as.character(run_year))) +
#   geom_density(alpha=0.6)#+
#   #geom_histogram(aes(y=..density..), alpha = 0.5, position = "identity")

over_5k_runs_test %>%
ggplot(., aes(x = min_per_km, y = as.character(year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Pace [min/km]", option = "C", direction = -1) +
  labs(title = 'Paces by year')


over_5k_runs_test %>%
  ggplot(., aes(x = min_per_km, y = as.character(run_class), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Pace [min/km]", option = "C", direction = -1) +
  labs(title = 'Paces by class')


## histogram by time of day overall then by year?
over_5k_runs_test %>%
  ggplot(., aes(x = hour)) +
  geom_histogram(stat = "count", breaks=c(1:24))+
  coord_polar() +
  scale_x_continuous("", limits = c(0, 24),
                     breaks = seq(0, 24), labels = seq(0,24)) +
  ylab("") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        axis.ticks.y = element_blank())


over_5k_runs_test %>%
  ggplot(., aes(x = hour)) +
  geom_histogram(stat = "count", breaks=c(1:24))+
  coord_polar() +
  scale_x_continuous("", limits = c(0, 24),
                     breaks = seq(0, 24), labels = seq(0,24)) +
  ylab("") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        axis.ticks.y = element_blank()) +
  facet_wrap(~year, nrow = 2)


## pie chart for run class
over_5k_runs_test %>%
  group_by(run_class) %>%
  summarise(count = n()) %>%
  # group_by(run_class) %>%
  mutate(percent = count / sum(count)) %>%
  ungroup %>%
  ggplot(., aes(x = "", y = percent, fill = run_class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()


over_5k_runs_test %>%
  group_by(year, run_class) %>%
  summarise(count = n()) %>%
  # group_by(run_class) %>%
  mutate(percent = count / sum(count)) %>%
  ungroup %>%
  ggplot(., aes(x = "", y = percent, fill = run_class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void() +
  facet_wrap(~year, nrow = 2)


# calendar heatmap
  ## gotta fiddle with colors
# over_5k_runs_test %>%
#   select(run_ymd, distance_k, time_min) %>%
#   mutate(year = year(run_ymd),
#          month = month(run_ymd, label = TRUE),
#          wkday = fct_relevel(wday(run_ymd, label=TRUE),
#                              c("Sun","Sat","Fri","Thu","Wed","Tue","Mon")),
#          day = day(run_ymd),
#          wk = format(run_ymd, "%W")) -> for_calendar
#
# for_calendar %>%


##### I AM NOT CAPTURING THE SUM HERE... look at WED in 2019...
  ### also there are duplicates for week/day/year because I do multiple activities.
  ## I need the SUM here
over_5k_runs_test %>%
  group_by(year, month, wk, wkday) %>%
  summarize(distance_k_sum = sum(distance_k)) %>%
  ggplot(., aes(wk, wkday, fill=distance_k_sum)) +
  geom_tile(color='black') +
  geom_text(aes(label=round(distance_k_sum)), size=3) +
  labs(x='',
       y='',
       title="Distance by day") +
  scale_fill_gradient(low="blue", high="red") +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_rect("grey92")) +
  facet_grid(year~month, scales="free", space="free")


over_5k_runs_test %>%
  group_by(year, month, wk, wkday) %>%
  summarize(time_sum = sum(time_min)) %>%
  ggplot(., aes(wk, wkday, fill=time_sum)) +
  geom_tile(color='black') +
  geom_text(aes(label=round(time_sum)), size=3) +
  labs(x='',
       y='',
       title="Time by day") +
  scale_fill_gradient(low="blue", high="red") +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_rect("grey92")) +
  facet_grid(year~month, scales="free", space="free")


## some additional quick functions to make:
  ## by_year

## I should get the above into the targets pipeline and try it out.

# this goes and pulls from the API again, but now with just particular ids
## this needs to be purrr::map'd
# df_meas <- read_activity_stream(
#   list_df_act %>% pluck("Run") %>% filter(manual == FALSE) %>% pull(id),
#   my_sig) #, pattern = map(act_ids)

list_df_act %>%
  pluck("Run") %>%
  filter(manual == FALSE) %>%
  activity_by_year(., "2019") %>%
  pull(id) %>%
  purrr::map(.x = ., .f = read_activity_stream, sig = my_sig) -> df_meas

  ## why are we binding this instead of iterating over?
df_meas_all <- bind_rows(df_meas)
df_meas_wide <- pivot_wider(df_meas_all, names_from = type, values_from = data)
df_meas_pro <- meas_pro(df_meas_wide)
df_meas_norm <- meas_norm(df_meas_pro)

  ## not sure what this one is supposed to do... with its magic -604
df_meas_rel <- meas_rel(df_act, df_meas_pro)

gg_meas <- vis_meas(df_meas_pro)
gg_meas_save <- save_gg_meas(gg_meas)


# for(i in act_ids){
#   act_url <- parse_url(stringr::str_glue(
#     "https://www.strava.com/api/v3/activities/{i}/streams"))
#   access_token <- my_sig$credentials$access_token[[1]]
#
#   r <- modify_url(
#     act_url,
#     query = list(
#       access_token = access_token,
#       keys = str_glue(
#         "distance,time,latlng,altitude,velocity_smooth,heartrate,cadence,",
#         "watts,temp,moving,grade_smooth"))) %>% GET()
#
#   stop_for_status(r)
#
#   print(i)
# }
