## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## GII Data
## src/sql_tools.R
## Davide Bonaglia - revision
## 2 February 2023
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script contains basic read and write SQL tools for the GIIDB.

## It has the following dependencies:
##  - assertr
##  - DBI
##  - dplyr
##  - glue
##  - purrr
##  - stringr
##  - tibble


### START CODE ###


# read_id -----------------------------------------------------------------------------------------
## This function reads indicator id data from the gii.id view and produces a list object containing 
## among other data its NAME, NAME_ABBR, NAME_UNITS, NUM, and EXPORT.

## con =    database connection object using DBI::dbConnect()
## giiyr =  GII report year as a numeric or a string in format YYYY; constrained to be greater 
##          than 2020
## code =   indicator code as a string

read_id <- function(con, giiyr, code) {
  
  ## Assertions
  stopifnot(
    DBI::dbIsValid(con),
    length(giiyr)==1 & (is.numeric(giiyr) | is.character(giiyr)),
    length(code)==1 & is.character(code)
  )
  if (is.character(giiyr)) {giiyr <- as.integer(giiyr)}
  endyr <- as.integer(format(Sys.Date(), "%Y"))
  if (giiyr<2020 | giiyr>endyr) {
    stop(glue::glue("giiyr is out of range, please try a year between [2020, {endyr}]."))
  }
  
  cat(giiyr, "--", code, "\n", sep=" ")
  
  ## Import indicator id data
  # cat("  Read indicator data\n", sep=" ")
  df <- DBI::dbGetQuery(con, glue::glue("
    SELECT DISTINCT *
    FROM		        `gii`.`id`
    WHERE           `GIIYR`={giiyr}
                    AND `CODE`='{code}'
    LIMIT 1
    ;
  "))
  
  if (nrow(df)==0) {
    stop(glue::glue('The combination of giiyr = "{giiyr}" and code = "{code}" does not exist.'))
  }
  
  return(as.list(df))
}


# read_tbl ----------------------------------------------------------------------------------------
## This function reads GIIDB data from the specified table or view and produces dataframe 
## containing all available columns.

## con    = database connection object using DBI::dbConnect()
## tbl    = database table name as a string
## giiyr  = GII report year as a numeric or a string in format YYYY; constrained to be greater 
##          than 2020
## code   = indicator code as a string

read_tbl <- function(con, tbl, giiyr=NULL, code=NULL) {
  
  ## Assertions
  stopifnot(
    DBI::dbIsValid(con),
    length(tbl)==1 & is.character(tbl),
    is.null(giiyr) | (length(giiyr)==1 & (is.numeric(giiyr) | is.character(giiyr))),
    is.null(code) | (length(code)==1 & is.character(code))
  )
  if (is.null(giiyr) & !is.null(code)) {
    stop('When "code" is defined, "giiyr" must also be defined.')
  }
  if (is.character(giiyr)) {giiyr <- as.integer(giiyr)}
  if (!is.null(giiyr)) {
    endyr <- as.integer(format(Sys.Date(), "%Y"))
    if (giiyr<2020 | giiyr>endyr) {
      stop(glue::glue("giiyr is out of range, please try a year between [2020, {endyr}]."))
    }
  }
  
  ## Import indicator id data
  # cat("  Read indicator data\n", sep=" ")
  if (!is.null(giiyr) & !is.null(code)) {
    cat(tbl, "--", giiyr, "--", code, "\n", sep=" ")
    sql <- glue::glue("
        SELECT DISTINCT *
        FROM		        `gii`.`{tbl}`
        WHERE           `GIIYR`={giiyr}
                        AND `CODE`='{code}'
        ;
      ")
  } else if (!is.null(giiyr)) {
    cat(tbl, "--", giiyr, "\n", sep=" ")
    sql <- glue::glue("
        SELECT DISTINCT *
        FROM		        `gii`.`{tbl}`
        WHERE           `GIIYR`={giiyr}
        ;
      ")
  } else {
    cat(tbl, "\n", sep=" ")
    sql <- glue::glue("
        SELECT DISTINCT *
        FROM		        `gii`.`{tbl}`
        ;
      ")
  }
  df <- DBI::dbGetQuery(con, sql)
  
  if (nrow(df)==0) {
    stop(glue::glue('The combination of giiyr = "{giiyr}" and code = "{code}" does not exist in gii.{tbl}.  Please try again.'))
  }
  
  return(df)
}


# insert ------------------------------------------------------------------------------------------
## This function writes temporary data to the MariaDB gii schema and then inserts it into the 
## appropriate gii table.

## con =  database connection object using DBI::dbConnect()
## tbl =  database table name as a string or a list of strings
## data = data to be inserted as a dataframe or a list of dataframes, where all columns of all 
##        dataframes must match those in the corresponding database table


insert <- function(con, tbl, data) {

  ## Assertions
  ## ... con
  if (!DBI::dbIsValid(con)) {
    stop("con is not a valid DBI connection object.")
  }
  
  ## ... tbl
  assert_tbl <- function(tbl) {
    (is.character(tbl) & length(tbl)==1)
  }
  if (!(assert_tbl(tbl) | (is.list(tbl) & all(purrr::map_lgl(tbl, assert_tbl))))) {
    stop("tbl is not a string or a list of strings.")
  }
  
  ## ... data
  if (!(is.data.frame(data) | (is.list(data) & all(purrr::map_lgl(data, is.data.frame))))) {
    stop("data is not a dataframe or a list of dataframes.")
  }
  
  ## ... length(tbl)==length(data)
  if (is.data.frame(data)) {
    if (length(tbl)!=1) {
      stop("tbl and data are not the same length.")
    }
  } else {
    if (length(tbl)!=length(data)) {
      stop("tbl and data are not the same length.")
    }
  }
  
  
  ## Choose method
  UseMethod("insert", data)
}


## data.frame method
insert.data.frame <- function(con, tbl, data) {
  
  ## Write temporary data to MariaDB gii.tmp
  ## NB - The default field types for the temporary table cause issues with the GII data; thus,
  ##      all string types must be reset to "TEXT CHARACTER SET utf8mb4".
  DBI::dbWriteTable(con,
                    name="tmp",
                    value=data,
                    row.names=FALSE, overwrite=TRUE, temporary=TRUE,
                    field.types=sub("VARCHAR\\(255\\)", "TEXT CHARACTER SET utf8mb4", 
                                    DBI::dbDataType(con, data)))
  
  ## Insert temporary data into table
  cols <- glue::glue_sql_collapse(names(data), sep=", ")
  eq <- data.frame(COL = names(data)) |>
    glue::glue_data("{COL} = tmp.{COL}") |>
    glue::glue_sql_collapse(sep=",\n    ")
  
  tryCatch({
    ## Commit write on success
    DBI::dbBegin(con)
    
    n <- DBI::dbExecute(con, glue::glue("
        INSERT INTO gii.{tbl} (
          {cols}
        )
        SELECT  *
        FROM    gii.tmp AS tmp
        ON DUPLICATE KEY UPDATE
          {eq}
        ;
      "))
    
    DBI::dbCommit(con)
    },
    ## Rollback on failure
    error = function(e) {
      DBI::dbRollback(con)
      stop(glue::glue("Upload failed.\n\n{e}"), call.=TRUE)
    },
    ## Drop temporary table
    finally = {
      DBI::dbExecute(con, DBI::SQL("
          DROP TEMPORARY TABLE IF EXISTS gii.tmp;
        "))
    }
  )

  cat("Insert into gii.", tbl, " successful [", prettyNum(n, big.mark=","), "]\n", sep="")
}


## itr method
## NB - This is a helper function for the list method below
insert.itr <- function(con, tbl, data) {
  
  ## Write temporary data to MariaDB gii.tmp
  ## NB - The default field types for the temporary table cause issues with the GII data; thus,
  ##      all string types must be reset to "TEXT CHARACTER SET utf8mb4".
  data <- structure(data, class=setdiff(class(data), c("itr")))
  DBI::dbWriteTable(con,
                    name="tmp",
                    value=data,
                    row.names=FALSE, overwrite=TRUE, temporary=TRUE,
                    field.types=sub("VARCHAR\\(255\\)", "TEXT CHARACTER SET utf8mb4", 
                                    DBI::dbDataType(con, data)))
  
  ## Insert temporary data into table
  cols <- glue::glue_sql_collapse(names(data), sep=", ")
  eq <- data.frame(COL = names(data)) |>
    glue::glue_data("{COL} = tmp.{COL}") |>
    glue::glue_sql_collapse(sep=",\n    ")
  
  n <- DBI::dbExecute(con, glue::glue("
          INSERT INTO gii.{tbl} (
            {cols}
          )
          SELECT  *
          FROM    gii.tmp AS tmp
          ON DUPLICATE KEY UPDATE
            {eq}
          ;
        "))
  
  ## Drop temporary table
  DBI::dbExecute(con, DBI::SQL("
    DROP TEMPORARY TABLE IF EXISTS gii.tmp;
  "))
  
  cat("[", prettyNum(n, big.mark=","), "]\n", sep="")
}


## list method
insert.list <- function(con, tbl, data) {

  ## Write data
  cat("Write data\n")
  tryCatch({
    ## Commit write on success
    DBI::dbBegin(con)
    
    purrr::walk2(tbl, data,
                 ~{
                   cat("  ... ", .x, "  ", sep="")
                   insert(con, tbl=.x, 
                          data=structure(.y, class=c("itr", class(.y))))
                 })
    
    DBI::dbCommit(con)
    },
    ## Rollback on failure
    error = function(e) {
      DBI::dbRollback(con)
      stop(glue::glue("Upload failed.\n\n{e}"), call.=TRUE)
    },
    ## Drop temporary table
    finally = {
      DBI::dbExecute(con, DBI::SQL("
          DROP TEMPORARY TABLE IF EXISTS gii.tmp;
        "))
    }
  )
}


# delete ------------------------------------------------------------------------------------------
## This function deletes outlier data from various GIIDB tables and returns the list of deleted
## ISO3 codes and, potentially, DATAYRs.

## con =    database connection object using DBI::dbConnect()
## tbl =    database table name as a string in set {indicator_prep, indicator_audit, outliers, raw_*}
## giiyr =  GII report year as a numeric or a string in format YYYY; constrained to be greater 
##          than 2020
## code =   indicator code as a string
## iso3 =   character vector of ISO3 codes in format AAA
## datayr = data year as a numeric or a string in format YYYY

delete <- function(con, tbl, giiyr, code=NULL, iso3, datayr=NULL) {
  
  ## Assertions
  ## ... con
  if (!DBI::dbIsValid(con)) {
    stop("con is not a valid DBI connection object.")
  }
  
  ## ... tbl
  if (!(tbl %in% c("indicator_prep","indicator_audit","outliers") | grepl("^raw_", tbl))) {
    stop("tbl is not a valid entry; please use one of {indicator_prep, indicator_audit, outliers, raw_*}.")
  }
  
  ## ... giiyr
  if (!(length(giiyr)==1 & (is.numeric(giiyr) | is.character(giiyr)))) {
    stop("giiyr is not a numeric or string value.")
  }
  if (is.character(giiyr)) {giiyr <- as.integer(giiyr)}
  endyr <- as.integer(format(Sys.Date(), "%Y"))
  if (giiyr<2020 | giiyr>endyr) {
    stop(glue::glue("giiyr is out of range, please try a year between [2020, {endyr}]."))
  }
  
  ## ... code
  if (grepl("^raw_", tbl)) {
    if (!is.null(code)) {
      stop("code should be NULL.")
    }
  } else {
    if (!(length(code)==1 & is.character(code))) {
      stop("code is not a string value.")
    }
  }
  
  ## ... iso3
  if (!is.character(iso3)) {
    stop("iso3 is not a character vector.")
  }
  if (!all(purrr::map_lgl(as.list(iso3), ~grepl("^[A-Z]{3}$", .x)))) {
    stop("iso3 value(s) are not valid.")
  }
  if (tbl=="indicator_prep") {
    if (length(iso3)!=1) {
      stop('iso3 must be a single string when tbl=="indicator_prep".')
    }
  }
  
  ## ... datayr
  if (tbl %in% c("indicator_audit","outliers") | grepl("^raw_", tbl)) {
    if (!is.null(datayr)) {
      stop('datayr must be NULL when tbl %in% c("indicator_audit","outliers","raw_*).')
    }
  } else {
    if (!is.numeric(datayr)) {
      stop("datayr is not a numeric vector.")
    }
    if (!all(purrr::map_lgl(as.list(datayr), ~grepl("^(19|20)\\d{2}$", .x)))) {
      stop("datayr value(s) are not valid.")
    }
  }
  
  
  ## Choose method
  if (grepl("^raw_", tbl)) {
    tbl_expr <- "raw"
  } else {
    tbl_expr <- tbl
  }
  switch(tbl_expr,
         "indicator_prep" = delete.datayr(con, tbl, giiyr, code, iso3, datayr),
         "indicator_audit" = delete.iso3(con, tbl, giiyr, code, iso3),
         "outliers" = delete.iso3(con, tbl, giiyr, code, iso3),
         "raw" = delete.iso3(con=con, tbl=tbl, giiyr=giiyr, iso3=iso3))
}


## iso3 method
delete.iso3 <- function(con, tbl, giiyr, code, iso3) {
  
  ## Insert temporary data into table
  iso3_txt <-  paste(iso3, collapse="','")
  iso3_txt <- paste0("'", iso3_txt, "'")
  
  tryCatch({
    ## Commit delete on success
    DBI::dbBegin(con)
    
    if (grepl("^raw_", tbl)) {
      df.iso3 <- DBI::dbGetQuery(con, glue::glue("
        DELETE IGNORE
        FROM 	    `gii`.{tbl}
        WHERE 	  `GIIYR`={giiyr}
                  AND `ISO3` IN ({iso3_txt})
        RETURNING `ISO3`
        ;
      "))
    } else {
      df.iso3 <- DBI::dbGetQuery(con, glue::glue("
        DELETE IGNORE
        FROM 	    `gii`.{tbl}
        WHERE 	  `GIIYR`={giiyr}
        		      AND `CODE`='{code}'
                  AND `ISO3` IN ({iso3_txt})
        RETURNING `ISO3`
        ;
      "))
    }
    
    DBI::dbCommit(con)
  },
  ## Rollback on failure
  error = function(e) {
    DBI::dbRollback(con)
    stop(glue::glue("Delete failed.\n\n{e}"), call.=TRUE)
  })
  
  iso3_return <- sort(unique(df.iso3$ISO3)) |>
    paste(collapse=", ")
  cat("Delete from gii.", tbl, " successful [", iso3_return, "]\n", sep="")
}


## datayr method
delete.datayr <- function(con, tbl, giiyr, code, iso3, datayr) {
  
  ## Insert temporary data into table
  datayr_txt <-  paste(datayr, collapse=",")
  
  tryCatch({
    ## Commit delete on success
    DBI::dbBegin(con)
    
    df.datayr <- DBI::dbGetQuery(con, glue::glue("
        DELETE IGNORE
        FROM 	    `gii`.{tbl}
        WHERE 	  `GIIYR`={giiyr}
        		      AND `CODE`='{code}'
                  AND `ISO3`='{iso3}'
                  AND `DATAYR` IN ({datayr_txt})
        RETURNING `DATAYR`
        ;
      "))
    
    DBI::dbCommit(con)
  },
  ## Rollback on failure
  error = function(e) {
    DBI::dbRollback(con)
    stop(glue::glue("Delete failed.\n\n{e}"), call.=TRUE)
  })
  
  datayr_return <- sort(unique(df.datayr$DATAYR)) |>
    paste(collapse=", ")
  cat("Delete from gii.", tbl, " successful for ", iso3, " in data year(s) [", datayr_return, "]\n", sep="")
}
