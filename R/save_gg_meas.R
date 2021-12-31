save_gg <- function(gg_meas,output_name, width = 7, height = 7, dpi = 300) {

  ggsave(output_name, gg_meas, width = width, height = height, dpi = dpi)

  return(output_name)
}
