# Name of App *(Give your app a short and informative title. Please adhere to our convention of Title Case without hyphens (e.g. My New App))*

MoveApps

Github repository: *github.com/yourAccount/Name-of-App* *(the link to the repository where the code of the app can be found must be provided)*

## Description
*Enter here the short description of the App that might also be used when filling out the description when submitting the App to Moveapps. This text is directly presented to Users that look through the list of Apps when compiling Workflows.*

## Documentation
*Enter here a detailed description of your App. What is it intended to be used for. Which steps of analyses are performed and how. Please be explicit about any detail that is important for use and understanding of the App and its outcomes.*

### Application scope
#### Generalization of the App
*State here if the App was developed for a specific species, taxa or taxonomic group, or to answer a specific question. This will help the user understand why the App might be producing non or odd results. Examples:*
This App was developed using data of birds. 
This App was developed using data of red deer. 
This App was developed for any taxonomic group. 
This App was developed to identify kill sites but could probably be used to identify any kind of locations cluster like a nest, den, drinking hole.

#### Ideal data properties
*State here the ideal data properties for this App to work. Examples:*
This App only is applicable to data that reflect range resident behavior. 
The data should have a fix rate of at least 30min. 
The App should work for any kind of data.

### Input type
*Indicate which type of input data the App requires.*

*Example*: `move2::move2_loc`

### Output type
*Indicate which type of output data the App produces to be passed on to subsequent apps.*

*Example:* `move2::move2_loc`

### Artefacts
*If the App creates artefacts (e.g. csv, pdf, jpeg, shapefiles, etc), please list them here and describe each.*

*Example:* `rest_overview.csv`: csv-file with Table of all rest site properties

### Settings 
*Please list and define all settings/parameters that the App requires to be set by the App user, if necessary including their unit. Please first state the Setting name the user encounters in the Settings menu defined in the appspecs.json, and between brackets the argument used in the R function to be able to identify it quickly in the code if needed.*

*Example:* `Radius of resting site` (radius): Defined radius the animal has to stay in for a given duration of time for it to be considered resting site. Unit: `metres`.

### Changes in output data
*State here how and if the App has modified the input data. Examples:*
The App adds to the input data the columns `Max_dist` and `Avg_dist`. 
The App filterers the input data as selected by the user. 
The output data is the outcome of the model applied to the input data. 
The input data remains unchanged.

### Most common errors
*Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.*

### Null or error handling
*Please indicate for each setting as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if settings/parameters are improperly set and any other important information that you find the user should be aware of.*

*Example:* **Setting `radius`:** If no radius AND no duration are given, the input data set is returned with a warning. If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set. 
