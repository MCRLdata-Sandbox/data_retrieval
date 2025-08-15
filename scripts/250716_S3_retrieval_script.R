## This script reads in data directly from AWS S3 buckets. It requires authorization
## via single sign-on (SSO) which is setup and enabled through the command line
## interface (CLI). This requires some work, and you must get read access to the
## MCRLdata pipeline. Please note that all interactions with AWS in this manner
## cost something, so should be used either sparingly or with a WPN.
##
## IMPORTANT - read above! This script will NOT work unless 1) you have authorized
## access to mcrl_data on AWS, 2) you have successfully set up SSO, and 3) you
## are signed in via CLI with proper credentials!
##
## 2025-07-01
## Peter Regier
##
# ########### #
# ########### #
## Start off clean
rm(list = ls())

## Load packages
require(pacman)
p_load(paws.storage,
       tictoc,
       tidyverse)

# Set up AWS credentials using your profile
Sys.setenv("AWS_PROFILE" = "mcrl_data")
Sys.setenv("AWS_REGION" = "us-west-2")

# Initialize S3 client
s3 <- paws.storage::s3()
my_bucket <- "athena-mcrl-data"

# Function to list files created within the last x days
list_recent_files <- function(bucket_name, no_days) {
  # Initialize S3 client
  s3 <- paws.storage::s3()
  
  # Get the current date and time
  current_time <- Sys.time()
  
  # Get the list of objects in the bucket
  objects <- s3$list_objects_v2(Bucket = bucket_name)$Contents
  
  # Filter objects created within the last 2 days
  recent_files <- lapply(objects, function(object) {
    object_time <- as.POSIXct(object$LastModified, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    key <- object$Key
    
    # Check if the object is within the last 2 days and satisfies the grepl filters
    if (difftime(current_time, object_time, units = "days") <= no_days &&
        grepl("\\.csv$", key) &&
        !grepl("\\.csv\\.metadata$", key) &&
        !grepl("Query", key) &&
        !grepl("Unsaved", key)) {
      return(key)
    }
    return(NULL)
  })
  
  # Remove NULL values from the list
  recent_files <- Filter(Negate(is.null), recent_files)
  return(recent_files)
}

tic("run the process!")
recent_files <- list_recent_files(my_bucket, 14)

# Function to inspect file metadata or file content and return a tibble
inspect_file_metadata <- function(bucket_name, file_keys) {
  
  # Initialize S3 client
  s3 <- paws.storage::s3()
  
  # Iterate through file keys and collect results
  file_inspections <- lapply(file_keys, function(key) {
    # Try retrieving metadata and inspect content
    preview <- NULL
    tryCatch({
      # Retrieve the file content
      file_content <- s3$get_object(Bucket = bucket_name, Key = key)
      
      # Parse the file content and extract headers (if readable as CSV)
      content <- rawToChar(file_content$Body)
      preview <- tryCatch({
        colnames(read_csv(content, n_max = 5))  # Extract headers
      }, error = function(e) {
        NULL
      })
    }, error = function(e) {
      message(paste("Error processing file:", key))
    })
    
    # Return a named list for each file
    list(
      file_key = key,
      cols = if (!is.null(preview)) paste(preview, collapse = ", ") else NA_character_
    )
  })
  
  # Combine results into a tibble
  result_tibble <- tibble(
    file_key = sapply(file_inspections, function(x) x$file_key),
    cols = sapply(file_inspections, function(x) x$cols)
  )
  
  return(result_tibble)
}

file_metadata <- inspect_file_metadata(my_bucket, recent_files)

# Function to read a single CSV file from S3
read_s3_csv <- function(file_key) {
  tryCatch({
    # Fetch the file content from S3
    file_content <- s3$get_object(Bucket = my_bucket, Key = file_key)
    
    # Convert raw content to character and parse as CSV
    content <- rawToChar(file_content$Body)
    
    message(paste("reading", file_key))
    
    read_csv(content)  # Use readr to parse the CSV data
  }, error = function(e) {
    message(paste("Error reading file:", file_key))
    return(NULL)
  })
}

pull_data_by_header <- function(col_to_find){
  
  tic("read data")
  x <- file_metadata %>% 
    filter(grepl(col_to_find, cols, perl = TRUE)) %>% #perl enables regex eval
    pull(file_key) %>% 
    map(read_s3_csv) %>% 
    keep(~ nrow(.x) > 0) %>%  # Remove list items with nrow = 0 
    bind_rows()
  toc()
  
  return(x)
}

cdom <- pull_data_by_header("cdom")
co2 <- pull_data_by_header("pco2_water")
ph <- pull_data_by_header("\\bqc_ph\\b") #careful regex here
adcp <- pull_data_by_header("maxu_qc")
ctd <- pull_data_by_header("qc_salinity")
tidegauge <- pull_data_by_header("qc_water_level")

toc()







