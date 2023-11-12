## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## GII Data
## src/get_config.R
## Jack Gregory & Davide Bonaglia
## 27 November 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script simplifies access to the GIIDB credentials using the config package.

## It has the following dependencies:
##  - config

### START CODE ###


# get_config --------------------------------------------------------------------------------------
## Access credentials from within specified YAML file.

## config = Name of the configuration as a string
## file = YAML config file as a string; default is "config.yml" located in the root directory

## Define config_get() function
get_config <- function(config, file="config.yml") {
  
  ## Assertions
  stopifnot(
    is.character(config),
    is.character(file)
  )
  
  ## Get config
  Sys.setenv(R_CONFIG_ACTIVE=config)
  l.out <- config::get(file=file)
  Sys.setenv(R_CONFIG_ACTIVE="default")
  
  ## Return
  if (!("user" %in% names(l.out))) {
    stop(paste0('"', user, '" is not a valid username.\nCheck <',
                file, "> in working directory for list of valid users."))
  }
  return(l.out)
}


### END CODE ###

