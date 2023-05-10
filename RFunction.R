library('move')
library('lubridate')

## The parameter "data" is reserved for the data object passed on from the previous app

## to display messages to the user in the log file of the App in MoveApps one can use the function from the logger.R file: 
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

rFunction = function(data, sdk, year, ...) {
  logger.info(paste("Welcome to the", sdk))
  if (any(lubridate::year(data@timestamps) == year)) { 
    data[lubridate::year(data@timestamps) == year]
  } else {
    NULL
  }
}
