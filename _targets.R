source("libraries.R")

walk(dir_ls("R"), source)

# I should use this only for generate analyses and viz
# the report should be manual because it requires interpretation of figures

# list(
#   tar_target(athlete_id, Sys.getenv("ATHLETE_ID")), # get your athlete id: we can make this a secret too (for privacy)
#
#   tar_target(act_col_types,
#     list(
#       moving = col_logical(), velocity_smooth = col_number(),
#       grade_smooth = col_number(), distance = col_number(),
#       altitude = col_number(), heartrate = col_integer(), time = col_integer(),
#       lat = col_number(), lng = col_number(), cadence = col_integer(),
#       watts = col_integer()
#     )
#   ),
#
#   tar_target(my_app, define_strava_app()),
#   tar_target(my_endpoint, define_strava_endpoint()),
#   tar_target(
#     my_sig, define_strava_sig(my_endpoint, my_app),
#     cue = tar_cue(mode = "always")),
#
#   tar_target(df_act_raw, read_all_activities(my_sig)),
#   tar_target(df_act, pre_process_act(df_act_raw, athlete_id)),
#   tar_target(act_ids, pull(distinct(df_act, id))),
#
#   # Dynamic branching
#   tar_target(
#     df_meas, read_activity_stream(act_ids, my_sig), pattern = map(act_ids),
#     cue = tar_cue(mode = "never")),
#
#   tar_target(df_meas_all, bind_rows(df_meas)),
#   tar_target(df_meas_wide, meas_wide(df_meas_all)),
#   tar_target(df_meas_pro, meas_pro(df_meas_wide)),
#   tar_target(df_meas_norm, meas_norm(df_meas_pro)),
#   tar_target(df_meas_rel, meas_rel(df_act, df_meas_pro)),
#   tar_target(gg_meas, vis_meas(df_meas_pro)),
#   tar_target(gg_meas_save, save_gg_meas(gg_meas), format = "file"),
#
#   tar_render(strava_report, "scrape_strava.Rmd"),
#   tar_render(
#     strava_post, "scrape_strava.Rmd",
#     output_format = distill::distill_article(),
#     output_file = "scrape_strava_post.html"),
#   tar_render(
#     strava_readme, "scrape_strava.Rmd", output_format = "md_document",
#     output_file = "README.md")
# )



list(
  tar_target(my_app, define_strava_app()),
  tar_target(my_endpoint, define_strava_endpoint()),
  tar_target(
    my_sig, define_strava_sig(my_endpoint, my_app),
    cue = tar_cue(mode = "always")),

  tar_target(df_act_raw, read_all_activities(my_sig)),
  tar_target(df_act, pre_process_act(df_act_raw)),
  tar_target(runs_with_moving_time, {\(x) x %>%
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
      ungroup()}(df_act)),

  tar_target(distace_time_viz_ggplot, distace_time_viz(runs_with_moving_time)),
  tar_target(distace_time_viz_png, save_gg(distace_time_viz_ggplot,"./figures/distace_time_viz_ggplot.png"), format = "file"),

  tar_target(distace_time_viz_ggplot_facet, distace_time_viz_year_facet(runs_with_moving_time)),
  tar_target(distace_time_viz_png_facet, save_gg(distace_time_viz_ggplot_facet,"./figures/distace_time_viz_ggplot_facet.png"), format = "file"),

  tar_target(distace_time_viz_race_ggplot_facet, distace_time_viz_race_year_facet(runs_with_moving_time)),
  tar_target(distace_time_viz_race_png_facet, save_gg(distace_time_viz_race_ggplot_facet,"./figures/distace_time_viz_race_ggplot_facet.png"), format = "file"),

  tar_target(paces_by_year_viz_ggplot, paces_by_year(runs_with_moving_time)),
  tar_target(paces_by_year_viz_png, save_gg(paces_by_year_viz_ggplot,"./figures/paces_by_year_viz_ggplot.png"), format = "file"),

  tar_target(clock_ggplot, clock_24_hours(runs_with_moving_time)),
  tar_target(clock_png, save_gg(clock_ggplot,"./figures/clock_ggplot.png"), format = "file"),

  tar_target(clock_ggplot_facet, clock_24_hours_facet_year(runs_with_moving_time)),
  tar_target(clock_png_facet, save_gg(clock_ggplot_facet,"./figures/clock_ggplot_facet.png"), format = "file"),

  tar_target(proportion_runs_ggplot, proportion_run_class(runs_with_moving_time)),
  tar_target(proportion_runs_png, save_gg(proportion_runs_ggplot,"./figures/proportion_runs_ggplot.png"), format = "file"),

  tar_target(proportion_runs_ggplot_facet, proportion_run_class_facet_year(runs_with_moving_time)),
  tar_target(proportion_runs_png_facet, save_gg(proportion_runs_ggplot_facet,"./figures/proportion_runs_ggplot_facet.png"), format = "file"),

  tar_target(calendar_heatmap_distance_ggplot, calendar_heatmap_distance(runs_with_moving_time)),
  tar_target(calendar_heatmap_distance_png, save_gg(calendar_heatmap_distance_ggplot,"./figures/calendar_heatmap_distance_ggplot.png", width = 12, height = 8, dpi = 400), format = "file"),

  tar_target(calendar_heatmap_time_ggplot, calendar_heatmap_time(runs_with_moving_time)),
  tar_target(calendar_heatmap_time_png, save_gg(calendar_heatmap_time_ggplot,"./figures/calendar_heatmap_time_ggplot.png", width = 12, height = 8, dpi = 400), format = "file"),

  tar_target(calendar_heatmap_pace_ggplot, calendar_heatmap_pace(runs_with_moving_time)),
  tar_target(calendar_heatmap_pace_png, save_gg(calendar_heatmap_pace_ggplot,"./figures/calendar_heatmap_pace_ggplot.png", width = 12, height = 8, dpi = 400), format = "file")


)
