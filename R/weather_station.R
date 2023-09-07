build_weather_station <- function(...) {
  out <- list()
  
  args <- list(...)
  for (i in seq_along(args)) {
    # Add additional parameters to the right spot in the list
    name <- names(args)[i]
    value <- args[[i]]
    out[[name]] <- value
  }
  
  class(out) <- "weather_station"
  
  out
}