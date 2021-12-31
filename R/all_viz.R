distace_time_viz <- function(activity_df){

  activity_df %>%
  ggplot(., aes(x = distance_k, y = time_min)) +
    geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) +
    geom_point(aes(fill = run_class), alpha = .5, size = 3, shape = 21, color = "black", stroke = 1) +
    theme_minimal() +
    scale_color_carto_d(palette = "Vivid") +
    xlab("Distance (kilometers)") +
    ylab("Time (minutes)")


}


distace_time_viz_year_facet <- function(activity_df){

  activity_df %>%
  ggplot(., aes(x = distance_k, y = time_min)) +
    facet_wrap(~year,nrow=2) +
    geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) +
    geom_point(aes(fill = run_class), alpha = .5, size = 3, shape = 21, color = "black", stroke = 1) +
    theme_minimal() +
    scale_color_carto_d(palette = "Vivid")
    xlab("Distance (kilometers)") +
    ylab("Time (minutes)")


}


paces_by_year <- function(activity_df){

  activity_df %>%
  ggplot(., aes(x = min_per_km, y = as.character(year), fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    theme_minimal() +
    scale_fill_viridis_c(name = "Pace [min/km]", option = "C", direction = -1) +
    labs(title = 'Paces by year') +
    xlab("Minutes per kilometer") +
    ylab("Year")

}


clock_24_hours <- function(activity_df){

  activity_df %>%
    ggplot(., aes(x = hour)) +
    geom_histogram(stat = "count", breaks=c(1:24), color = "black", fill = "firebrick3")+
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
}


clock_24_hours_facet_year <- function(activity_df){

  activity_df %>%
    ggplot(., aes(x = hour)) +
    geom_histogram(stat = "count", breaks=c(1:24), color = "black", fill = "firebrick3")+
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
}


proportion_run_class <- function(activity_df){

  activity_df %>%
    group_by(run_class) %>%
    summarise(count = n()) %>%
    mutate(percent = count / sum(count)) %>%
    ungroup %>%
    ggplot(., aes(x = "", y = percent, fill = run_class)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    scale_fill_carto_d(palette = "Vivid") +
    theme_void()

}


proportion_run_class_facet_year <- function(activity_df){

  activity_df %>%
    group_by(run_class) %>%
    summarise(count = n()) %>%
    mutate(percent = count / sum(count)) %>%
    ungroup %>%
    ggplot(., aes(x = "", y = percent, fill = run_class)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    scale_fill_carto_d(palette = "Vivid") +
    theme_void() +
    facet_wrap(~year, nrow = 2)

}


calendar_heatmap_distance <- function(activity_df){

  activity_df %>%
    group_by(year, month, wk, wkday) %>%
    summarize(distance_k_sum = sum(distance_k)) %>%
    ggplot(., aes(wk, wkday, fill=distance_k_sum)) +
    geom_tile(color='white') +
    geom_text(aes(label=round(distance_k_sum)), size=2.5, color = "black") +
    labs(x='',
         y='',
         title="Distance by day") +
    scale_fill_carto_c(palette="SunsetDark") +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          strip.background = element_rect("grey92")) +
    facet_grid(year~month, scales="free", space="free")
}


calendar_heatmap_time <- function(activity_df){

  activity_df %>%
    group_by(year, month, wk, wkday) %>%
    summarize(time_sum = sum(time_min)) %>%
    ggplot(., aes(wk, wkday, fill=time_sum)) +
    geom_tile(color='white') +
    geom_text(aes(label=round(time_sum)), size=2, color = "black") +
    labs(x='',
         y='',
         title="Time by day") +
    scale_fill_carto_c(palette="SunsetDark") +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          strip.background = element_rect("grey92")) +
    facet_grid(year~month, scales="free", space="free")
}

calendar_heatmap_pace <- function(activity_df){

  activity_df %>%
    group_by(year, month, wk, wkday) %>%
    summarize(pace_mean = mean(min_per_km)) %>%
    ggplot(., aes(wk, wkday, fill=2^-pace_mean)) +
    geom_tile(color='white') +
    geom_text(aes(label=round(pace_mean,digits = 1)), size=2, color = "black") +
    labs(x='',
         y='',
         title="Time by day") +
    scale_fill_carto_c(palette="Tropic", direction = 1) +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          strip.background = element_rect("grey92"),
          legend.position = "none") +
    facet_grid(year~month, scales="free", space="free")
}

