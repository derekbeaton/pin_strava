# non-target based

## bundle these together in a more clever way.
source("libraries.R")
walk(dir_ls("R"), source)

my_app <- define_strava_app()

my_endpoint <- define_strava_endpoint()

my_sig <- define_strava_sig(my_endpoint, my_app)
# ,cue = tar_cue(mode = "always")

df_act_raw <- read_all_activities(my_sig)

df_act <- pre_process_act(df_act_raw)

runs_with_moving_time <- {\(x) x %>%
  pluck("Run") %>%
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
  ungroup()}(df_act)





#
# runs_with_moving_time %>%
#   ggplot(., aes(x = distance_k, y = time_min)) +
#   geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
#   stat_poly_eq(formula = y ~ x,
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
#                parse = TRUE) +
#   geom_point(aes(fill = run_class), alpha = .5, size = 3, shape = 21, color = "black", stroke = 1) +
#   theme_minimal() +
#   scale_color_carto_d(palette = "Vivid")
#
#
# runs_with_moving_time %>%
#   ggplot(., aes(x = distance_k, y = time_min)) +
#   facet_wrap(~year,nrow=2) +
#   geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
#   stat_poly_eq(formula = y ~ x,
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
#                parse = TRUE) +
#   geom_point(aes(fill = run_class), alpha = .5, size = 3, shape = 21, color = "black", stroke = 1) +
#   theme_minimal() +
#   scale_color_carto_d(palette = "Vivid")
#
#
# runs_with_moving_time %>%
# ggplot(., aes(x = min_per_km, y = as.character(year), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   theme_minimal() +
#   scale_fill_viridis_c(name = "Pace [min/km]", option = "C", direction = -1) +
#   labs(title = 'Paces by year')
#
#
# ## histogram by time of day overall then by year?
#   ## clean up a bit
# runs_with_moving_time %>%
#   ggplot(., aes(x = hour)) +
#   geom_histogram(stat = "count", breaks=c(1:24), color = "black", fill = "firebrick3")+
#   coord_polar() +
#   scale_x_continuous("", limits = c(0, 24),
#                      breaks = seq(0, 24), labels = seq(0,24)) +
#   ylab("") +
#   theme_bw() +
#   theme(panel.border = element_blank(),
#         legend.key = element_blank(),
#         axis.text.y = element_blank(),
#         panel.grid  = element_blank(),
#         axis.ticks.y = element_blank())
#
#
#
# runs_with_moving_time %>%
#   ggplot(., aes(x = hour)) +
#   geom_histogram(stat = "count", breaks=c(1:24), color = "black", fill = "firebrick3")+
#   coord_polar() +
#   scale_x_continuous("", limits = c(0, 24),
#                      breaks = seq(0, 24), labels = seq(0,24)) +
#   ylab("") +
#   theme_bw() +
#   theme(panel.border = element_blank(),
#         legend.key = element_blank(),
#         axis.text.y = element_blank(),
#         panel.grid  = element_blank(),
#         axis.ticks.y = element_blank()) +
#   facet_wrap(~year, nrow = 2)
#
#
# ## pie chart for run class
# runs_with_moving_time %>%
#   group_by(run_class) %>%
#   summarise(count = n()) %>%
#   mutate(percent = count / sum(count)) %>%
#   ungroup %>%
#   ggplot(., aes(x = "", y = percent, fill = run_class)) +
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)+
#   scale_fill_carto_d(palette = "Vivid") +
#   theme_void()
#
#
# runs_with_moving_time %>%
#   group_by(year, run_class) %>%
#   summarise(count = n()) %>%
#   mutate(percent = count / sum(count)) %>%
#   ungroup %>%
#   ggplot(., aes(x = "", y = percent, fill = run_class)) +
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0)+
#   scale_fill_carto_d(palette = "Vivid") +
#   theme_void() +
#   facet_wrap(~year, nrow = 2)
#
#
#
# runs_with_moving_time %>%
#   group_by(year, month, wk, wkday) %>%
#   summarize(distance_k_sum = sum(distance_k)) %>%
#   ggplot(., aes(wk, wkday, fill=distance_k_sum)) +
#   geom_tile(color='white') +
#   geom_text(aes(label=round(distance_k_sum)), size=2.5, color = "black") +
#   labs(x='',
#        y='',
#        title="Distance by day") +
#   scale_fill_carto_c(palette="SunsetDark") +
#   theme(panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         strip.background = element_rect("grey92")) +
#   facet_grid(year~month, scales="free", space="free")
#
#
# runs_with_moving_time %>%
#   group_by(year, month, wk, wkday) %>%
#   summarize(time_sum = sum(time_min)) %>%
#   ggplot(., aes(wk, wkday, fill=time_sum)) +
#   geom_tile(color='white') +
#   geom_text(aes(label=round(time_sum)), size=2, color = "black") +
#   labs(x='',
#        y='',
#        title="Time by day") +
#   scale_fill_carto_c(palette="SunsetDark") +
#   theme(panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         strip.background = element_rect("grey92")) +
#   facet_grid(year~month, scales="free", space="free")
#
#
# runs_with_moving_time %>%
#   group_by(year, month, wk, wkday) %>%
#   summarize(pace_mean = mean(min_per_km)) %>%
#   ggplot(., aes(wk, wkday, fill=2^-pace_mean)) +
#   geom_tile(color='white') +
#   geom_text(aes(label=round(pace_mean,digits = 1)), size=2, color = "black") +
#   labs(x='',
#        y='',
#        title="Time by day") +
#   scale_fill_carto_c(palette="Tropic", direction = 1) +
#   theme(panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         strip.background = element_rect("grey92"),
#         legend.position = "none") +
#   facet_grid(year~month, scales="free", space="free")






## some additional quick functions to make:
  ## by_year

## I should get the above into the targets pipeline and try it out.

# this goes and pulls from the API again, but now with just particular ids
## this needs to be purrr::map'd
# df_meas <- read_activity_stream(
#   list_df_act %>% pluck("Run") %>% filter(manual == FALSE) %>% pull(id),
#   my_sig) #, pattern = map(act_ids)

# list_df_act %>%
#   pluck("Run") %>%
#   filter(manual == FALSE) %>%
#   activity_by_year(., "2019") %>%
#   pull(id) %>%
#   purrr::map(.x = ., .f = read_activity_stream, sig = my_sig) -> df_meas
#
#   ## why are we binding this instead of iterating over?
# df_meas_all <- bind_rows(df_meas)
# df_meas_wide <- pivot_wider(df_meas_all, names_from = type, values_from = data)
# df_meas_pro <- meas_pro(df_meas_wide)
# df_meas_norm <- meas_norm(df_meas_pro)
#
#   ## not sure what this one is supposed to do... with its magic -604
# df_meas_rel <- meas_rel(df_act, df_meas_pro)
#
# gg_meas <- vis_meas(df_meas_pro)
# gg_meas_save <- save_gg_meas(gg_meas)


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
