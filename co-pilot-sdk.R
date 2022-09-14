## Provided testing datasets in `data/raw`: 
## "input1_pigeons.rds", "input2_geese.rds", "input3_stork.rds", "input4_goat.rds"  
## for own data: file saved as a .rds containing a object of class MoveStack
inputFileName = "./data/raw/input1_pigeons.rds" 
## optionally change the output file name
outputFileName = "./data/output/output.rds" 

args <- list()

# enable better error reporting during development
options(error = function() traceback(3))

#################################################################
########################### Arguments ###########################
# The data parameter will be added automatically if input data is available
# The name of the field in the vector must be exactly the same as in the r function signature
# Example:
# rFunction = function(username, password)
# The parameter must look like:
#    args[["username"]] = "any-username"
#    args[["password"]] = "any-password"

# Add your arguments of your r-function here
args[["year"]] = 2014 

# tie everything together
# the following files will NOT bundled into the final app - they are just helpers for the SDK
source("src/common/logger.R")
source("src/io/app_files.R")
source("src/io/io_handler.R")
source("src/io/rds.R")
source("src/moveapps.R")
# this file comes from the app developer and will be bundled into the final app
source("RFunction.R")

# set your environment
Sys.setenv(SOURCE_FILE = inputFileName, OUTPUT_FILE = outputFile, ERROR_FILE="./data/output/error.log")

simulateMoveAppsRun(args)
