library('move')
library('lubridate')

## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.info(). Available levels are error(), warn(), info(), debug(), trace()

rFunction = function(data, sdk, year, ...) {
  logger.info(paste("Welcome to the", sdk))
  result <- if (any(lubridate::year(data@timestamps) == year)) { 
    data[lubridate::year(data@timestamps) == year]
  } else {
    NULL
  }
  if (!is.null(result)) {
    artifact <- appArtifactPath("plot.png")
    logger.info(paste("plotting to artifact:", artifact))
    png(artifact)
    plot(result)
    dev.off()
  } else {
    logger.warn("nothing to plot")
  }
  # provide my result to the next app in the MoveApps workflow
  return(result)
}
