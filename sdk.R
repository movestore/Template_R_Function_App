library(dotenv)
# You can control your local app development via environment variables.
# You can define things like input-data, app-configuration etc.
# Per default your environment is defined in `/.env`
load_dot_env()

# `./RFunction.R` is the home of your app code
# It is the only file which will be bundled into the final app on MoveApps
source("RFunction.R")

# Lets simulate running your app on MoveApps
source("src/common/runtime_configuration.R")
# This will parse a JSON file containing the concrete configuration of
# the app run. Per default the file `/app-configuration.json` will be parsed.
args <- configuration()

source("src/moveapps.R")
simulateMoveAppsRun(args)
