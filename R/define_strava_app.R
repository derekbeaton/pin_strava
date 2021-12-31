define_strava_app <- function() {
  if (Sys.getenv("STRAVA_KEY") == "" |
      Sys.getenv("STRAVA_SECRET") == "" |
      Sys.getenv("ATHLETE_ID") == "")
    stop(str_glue(
      "Please set system variables 'ATHLETE_ID', 'STRAVA_KEY', and 'STRAVA_SECRET' before ",
      "continuing. How you can create these variables is described here: ",
      "https://developers.strava.com/docs/getting-started/. ",
      "You can set the system variables with the `usethis::edit_r_environ` ",
      "function."))

  oauth_app(
    appname = "r_api",
    key = Sys.getenv("STRAVA_KEY"),
    secret = Sys.getenv("STRAVA_SECRET"))
}
