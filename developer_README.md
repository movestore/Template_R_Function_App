# MoveApps R Software Development Kit (SDK)

This documentation provides a short introduction to the [MoveApps](https://www.moveapps.org) **R SDK**.

As a first step, and before your read this, you should have forked this GitHub template to your personal space and named the repository as your App will be named in MoveApps.

# Overview

This template is designed according to a file structure that is necessary for your App to run in your local development environment similar to the way it will run in the MoveApps environment later. Please contain the structure and only change/add files as necessary for your App's functionality. See below which files can be changed and which should remain as is for simulation of the behaviour on MoveApps on your local system. A stepwise explanation below indicates the function and some background of each file and folder.

## File structure

(truncated)

```

.
├── Dockerfile
├── README.md
├── RFunction.R
├── Template_R_Function_App.Rproj
├── app-configuration.json
├── appspec.json
├── data
│   ├── local_app_files
│   ├── output
│   └── raw
│       ├── input1_greylgeese.rds
│       ├── input2_whitefgeese.rds
│       ├── input3_stork.rds
│       └── input4_goat.rds
├── renv.lock
├── sdk.R
├── src
│   ├── common
│   │   ├── logger.R
│   │   └── runtime_configuration.R
│   ├── io
│   │   ├── app_files.R
│   │   ├── io_handler.R
│   │   └── rds.R
│   └── moveapps.R
└── start-process.sh


```

1. `./RFunction.R`: This is the entrypoint for your App logic. MoveApps will call this function during a workflow execution which includes your App. **The file must be named `RFunction.R`, do not alter it!**
1. `./appspec.json`: This file defines the settings and metadata of your App, for details refer to the [MoveApps User Manual](https://docs.moveapps.org/#/appspec)
1. `./renv.lock`: Definition of the dependencies of your App. We use `renv` as library manager. Optional.
1. `./data/**`: Resources of the SDK
   1. `local_app_files/**`: Simulates the usage of [*app files*](https://docs.moveapps.org/#/auxiliary). You can put files into this folder to simulate an App run with provided/user-uploaded files. 
   1. `output/**`: If your App produces [*artefacts*](https://docs.moveapps.org/#/copilot-r-sdk?id=artefacts) they will be stored here.
   1. `raw/**`: Collection of sample App input data. You can use these samples to simulate an App run with real input.
1. `./sdk/**`: The (internal) MoveApps R SDK logic.
1. `./sdk.R`: The main entry point of the SDK. Use it to execute your App in your IDE.
1. TODO: `./tests/**`: Location for **Unit Tests**

### General notes

- get an overview with the help of the [user manual](https://docs.moveapps.org/#/create_app)
- files needed for your app:
  - your app code goes to `./RFunction.R`
  - setup your app arguments and your environment by adjusting `./appspec.json`
  - the documentation of your app goes to `./README.md`
- to run and test your app code locally in a simulated MoveApps environment adjust and execute the file `./co-pilot-sdk.R`
  - adjust the `inputFileName`
  - state the arguments of your function if present

### R packages management (optional)

The template is prepared to use [`renv` as a dependency manager](https://rstudio.github.io/renv/articles/renv.html) - but is disabled by default (_opt-in_).
You can [activate `renv` with `renv::activate()`](https://rstudio.github.io/renv/articles/renv.html#uninstalling-renv) and then use it in the [usual `renv` workflow](https://rstudio.github.io/renv/articles/renv.html#workflow).

### Docker support (optional)

- at the end your app will be executed on MoveApps in a Docker container.
- if you like you can test your app in the almost final environment by running your app locally in a docker container:

1. set a working title for your app by `export MY_MOVEAPPS_APP=hello-world` (in your terminal)
1. build the Docker image locally by `docker build -t $MY_MOVEAPPS_APP .` (in your terminal)
1. execute the image with `docker run --rm --name $MY_MOVEAPPS_APP -it $MY_MOVEAPPS_APP`
1. you will get a `bash` terminal of the running container. There you can get a R console by `R` or simply start your app by invoking `/home/moveapps/co-pilot-r/start-process.sh` inside the running container.
