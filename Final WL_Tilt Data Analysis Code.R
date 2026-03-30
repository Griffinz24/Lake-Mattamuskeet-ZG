#Compilation of all data management code
#Basic set up

#The steps for this are as follows.

#1. Ensure that all file paths are changed to the right directory for YOUR MACHINE, this will include 
# changing any variables that are expressed as "C:\\....."

#2. Ensure that you have installed all packages. If not, uncomment and run the below section (ctrl+shift+c)
# install.packages(stringr)
# install.packages(tidyverse)
# install.packages(readxl)
# install.packages(openxlsx)

#3. Begin running each section, pay very close attention to any notes and read comments BEFORE running.
# making sure you know where problems are occuring is very important.


#Part 1 Data Transfer from LoRa and Tilt Meter File Locations---------------------------------------------------------------------
#Setting up packages-------
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr)




# ---------- USER CONFIG ----------
file_date <- "20260227"


csv_folder   <- "C:\\Users\\zasha\\OneDrive - East Carolina University\\3110 Lab @ ECU (Etheridge) - Mattamuskeet Source Tracking\\Data\\Raw\\Tilt_Meters\\"
excel_folder <- "C:\\Users\\zasha\\OneDrive - East Carolina University\\3110 Lab @ ECU (Etheridge) - Mattamuskeet Source Tracking\\Data\\Analyses\\Flow"
csv_folder <- paste(csv_folder,file_date,sep="")
WL_folder <- "C:\\Users\\zasha\\OneDrive - East Carolina University\\3110 Lab @ ECU (Etheridge) - Mattamuskeet Source Tracking\\Data\\Raw\\Water_Level\\LoRa_Sensors"

csv_columns <- c("ISO 8601 Time", "Speed (cm/s)", "Heading (degrees)")
sheet_index <- 1


#look up table for pin names for each location.
#This needs ot be trimmed and adjusted once the actual location tokens are added
#CHECKCAPS
wl_lookup <- tribble(
  ~location_token,     ~device_id_pattern,
  "Outfall",    "pslb-5",
  "Outfall_Bridge",    "pslb-5",
  "Jarvis",      "pslb-7",
  "No4_Canal",      "pslb-6",
  "MI10_Headwaters",      "pslb-12",
  "MI10_Boundary",      "pslb-21",
  "Waupoppin",      "pslb-1", #This may be pslb-1 WHICH iS USING NOW --THERE IS ONLY 0/NA from 8/23-1/30 in pslb-1, no tilt meter collected after, no data append
  "Lake_Landing",      "pslb-2",#THIS SENSOR is giving problems
  "LL_West_Bound",      "pslb-9", 
  "East_Main",      "pslb-19",
  "West_Main",      "pslb-18",
  "MI-3_West",      "pslb-17",
  "Sandy_Dike",      "pslb-17",
  "Hodges_Canal",      "pslb-15",
  "MI2_West",      "pslb-16",
  "Rose_Bay",        "pslb-16", #<-This is NOT REALLLLLL
)



# ---------- START INITIALIZING THE FUNCTIONS ----------

safe_read_csv <- function(path) {
  tryCatch(
    read_csv(path, show_col_types = FALSE),
    error = function(e) {
      message("Failed to read CSV: ", path, " -- ", e$message)
      NULL
    }
  )
}

csv_files <- list.files(
  csv_folder,
  pattern = "Current\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(csv_files) == 0)
  stop("No CSV files found in: ", csv_folder)



safe_read_wl <- function(path) {
  message("Reading WL file as TAB-delimited: ", basename(path))
  
  tryCatch(
    readr::read_tsv(
      file = path,
      locale = readr::locale(encoding = "UTF-16LE"),
      show_col_types = FALSE,
      trim_ws = TRUE
    ),
    error = function(e) {
      message("Skipping corrupted WL file: ", basename(path))
      NULL
    }
  )
}


# ---------- PREP WATER LEVEL FILES (ALL, SAFE) ----------

wl_files <- list.files(
  WL_folder,
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(wl_files) == 0)
  stop("No WL files found in folder: ", WL_folder)

detect_delim <- function(path) {
  raw_lines <- readr::read_lines(
    path,
    locale = readr::locale(encoding = "UTF-16LE"),
    n_max = 5
  )
  
  if (any(grepl(";", raw_lines))) return(";")
  if (any(grepl("\t", raw_lines))) return("\t")
  if (any(grepl("\\|", raw_lines))) return("|")
  return(",")
}

clean_wl_names <- function(nm) {
  device <- stringr::str_extract(nm, "(?i)pslb[-_ ]?([0-9]+)") #0*([0-9]+)")
  
  # Normalize device IDs: pslb-1 → pslb-1, pslb01 → pslb-1, pslb_20 → pslb-20
  device_norm <- ifelse(
    is.na(device),
    NA_character_,
    paste0("pslb-", sub(".*?([0-9]+)$", "\\1", device))
  )
  
  # Build WL column names
  out <- ifelse(
    is.na(device_norm),
    nm,                      # leave untouched if no device ID
    paste0("WL_", device_norm)
  )
  
  return(out)
  
  
  # 
  # if (is.na(device)) return(nm)
  # 
  # # Normalize to a consistent exact-matchable form
  # device_norm <- paste0("pslb-", sub(".*?([0-9]+)$", "\\1", device))
  # 
  # # Return WL column name
  # paste0("WL_", device_norm)
  # 
  # # ifelse(is.na(device), nm,
  # #        paste0("WL_pslb-", sub(".*?([0-9]+)$", "\\1", device)))
}

wl_list <- lapply(wl_files, function(path) {
  delim <- detect_delim(path)
  message("Reading WL file: ", basename(path), " (delim='", delim, "')")
  
  
  # Read file safely
  df <- safe_read_wl(path)
  
  # If the file could not be read, skip it
  if (is.null(df)) {
    message("Skipping WL file due to read failure: ", basename(path))
    return(NULL)
  }
  
  
  if (is.null(df)) return(NULL)
  
  
  # Skip files with absurd header length
  if (any(nchar(names(df)) > 500)) {
    message("❌XXXXXXXXXXXXX Skipping WL file with corrupted header: ", basename(path))
    return(NULL)
  }
  
  # Clean column names
  names(df) <- clean_wl_names(names(df))
  
  # Standardize time column
  time_col <- names(df)[1]
  
  df %>%
    rename(`ISO 8601 Time` = all_of(time_col)) %>%
    mutate(`ISO 8601 Time` = as.POSIXct(
      `ISO 8601 Time`,
      format = "%m/%d/%Y %H:%M",
      tz = "UTC"
    ))
})

# Remove NULL entries
wl_list <- lapply(wl_files, function(path) {
  
  # 1. Read WL file safely
  df <- safe_read_wl(path)
  if (is.null(df)) return(NULL)
  
  # 2. Skip corrupted headers BEFORE renaming
  if (any(nchar(names(df)) > 500)) {
    message("❌ xxxxxxxxxxxxxxxxxx Skipping WL file with corrupted header: ", basename(path))
    return(NULL)
  }
  
  # 3. Clean names
  names(df) <- clean_wl_names(names(df))
  
  # 4. Identify timestamp column
  time_col <- names(df)[1]
  
  # 5. Rename timestamp column (NO PARSING HERE)
  df <- df %>% rename(`ISO 8601 Time` = all_of(time_col))
  
  return(df)
})


# Combine all WL data
wl_list <- wl_list[!sapply(wl_list, is.null)]
wl_raw <- bind_rows(wl_list)
message("Combined WL rows: ", nrow(wl_raw))
wl_raw <- wl_raw %>%
  mutate(`ISO 8601 Time` = as.POSIXct(`ISO 8601 Time`, tz = "UTC"))

# --- Standardize time column ---

excel_files_all <- list.files(
  excel_folder,
  pattern = "\\.xlsx?$",
  full.names = TRUE,
  ignore.case = TRUE
)

# ---------- LOCATION TOKEN CLEANING ----------

derive_location <- function(csv_path) {
  base <- tools::file_path_sans_ext(basename(csv_path))
  base <- str_remove(base, "\\(\\d+\\)")
  base <- str_replace_all(base, "__+", "_")
  base <- str_replace_all(base, "^_|_$", "")
  base <- str_remove(base, "_Current$")
  base <- str_replace(base, "^[0-9]+_", "")
  str_trim(base)
}

match_excels_for_location <- function(location_token, excel_paths) {
  if (is.na(location_token) || location_token == "")
    return(character(0))
  
  clean_names <- excel_paths %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    str_remove("\\s*\\(\\d+\\)$") %>%
    str_trim()
  
  excel_paths[clean_names == location_token]
}



# ---------- append_to_workbook() CLEANED ----------

append_to_workbook <- function(extracted_df, excel_path) {
  
  # Force numeric columns
  numeric_cols <- c(
    "water level (mm)", "Speed (cm/s)", "Heading (degrees)",
    "Velocity (ft/s)", "Water Level (ft NAVD 88)",
    "Area (ft^2)", "Flow Rate (cfs)"
  )
  
  for (col in numeric_cols) {
    if (col %in% names(extracted_df)) {
      extracted_df[[col]] <- suppressWarnings(as.numeric(extracted_df[[col]]))
    }
  }
  
  # Clean character columns only
  extracted_df <- extracted_df %>%
    mutate(across(where(is.character),
                  ~ ifelse(is.na(.), NA_character_, str_trim(.)))) %>%
    distinct()
  
  # Load or create workbook
  wb <- if (file.exists(excel_path)) loadWorkbook(excel_path) else createWorkbook()
  
  sheet_names <- names(wb)
  
  # Ensure sheet exists
  if (length(sheet_names) >= sheet_index) {
    target_sheet <- sheet_names[sheet_index]
  } else {
    new_sheet_name <- paste0("Sheet", sheet_index)
    addWorksheet(wb, new_sheet_name)
    target_sheet <- new_sheet_name
  }
  
  # Read existing sheet
  existing_df <- tryCatch(
    readWorkbook(excel_path, sheet = target_sheet),
    error = function(e) NULL
  )
  
  if (!is.null(existing_df)) {
    existing_df <- existing_df %>%
      mutate(across(everything(),
                    ~ ifelse(is.na(.), NA_character_, str_trim(as.character(.)))))
  }
  
  # Duplicate filtering
  if (!is.null(existing_df) && nrow(existing_df) > 0) {
    compare_cols <- intersect(names(extracted_df), names(existing_df))
    if (length(compare_cols) > 0) {
      before_n <- nrow(extracted_df)
      extracted_df <- anti_join(extracted_df, existing_df, by = compare_cols)
      dropped <- before_n - nrow(extracted_df)
      if (dropped > 0)
        message("Removed ", dropped, " duplicate row(s) for ", basename(excel_path))
    }
  }
  
  if (nrow(extracted_df) == 0) {
    message("No new rows to append for ", basename(excel_path))
    return(invisible(NULL))
  }
  
  # Append to sheet
  if (is.null(existing_df) || nrow(existing_df) == 0) {
    writeData(wb, sheet = target_sheet, x = extracted_df, startRow = 1, colNames = TRUE)
  } else {
    start_row <- nrow(existing_df) + 2
    writeData(wb, sheet = target_sheet, x = extracted_df, startRow = start_row, colNames = FALSE)
  }
  
  saveWorkbook(wb, excel_path, overwrite = TRUE)
  message("Appended ", nrow(extracted_df), " row(s) to ", basename(excel_path))
}

# ---------- MAIN LOOP ----------


for (csv_path in csv_files) {
  
  message("Processing CSV: ", basename(csv_path))
  csv_df <- safe_read_csv(csv_path)
  if (is.null(csv_df)) next
  
  # Location token
  location_token <- derive_location(csv_path)
  if (is.na(location_token) || location_token == "") {
    message("Could not derive location token from ", basename(csv_path))
    next
  }
  
  # WL device ID
  device_id <- wl_lookup$device_id_pattern[
    wl_lookup$location_token == location_token
  ]
  
  if (length(device_id) == 0) {
    message("No WL device ID found for location: ", location_token)
    next
  }
  
  
  # WL column
  wl_col <- names(wl_raw)[tolower(names(wl_raw)) == paste0("wl_", tolower(device_id))]
  
  
  #this attached to the end of the above variable.[str_detect(names(wl_raw),
  #fixed(device_id, ignore_case = TRUE))]
  
  if (length(wl_col) == 0) {
    message("No WL column found for device ID: ", device_id, " — filling with NA")
    wl_df <- wl_raw %>%
      select(`ISO 8601 Time`) %>%
      mutate(WL_mm = NA_real_)
  } else {
    # Build WL dataframe
    wl_df <- tryCatch({
      
      wl_raw %>%
        select(`ISO 8601 Time`, all_of(wl_col)) %>%
        rename(WL_raw = all_of(wl_col)) %>%
        mutate(
          WL_raw = as.character(WL_raw),
          #WL_raw = stringi::stri_trans_general(WL_raw, "Any-ASCII"),
          #WL_raw = str_replace_all(WL_raw, "\\s+", " "),
          WL_raw = str_trim(WL_raw),
          
          # remove the "mm" text
          WL_raw = str_remove(WL_raw, "mm"),
          
          # extract numeric value
          WL_mm = suppressWarnings(as.numeric(str_trim(WL_raw)))
        ) %>%
        select(`ISO 8601 Time`, WL_mm)
      
      
      # numeric_value = suppressWarnings(
      #   as.numeric(str_extract(WL_raw, "[0-9]+\\.?[0-9]*"))
      # ),
      # 
      # unit = case_when(
      #   str_detect(WL_raw, "mm") ~ "mm",
      #   str_detect(WL_raw, "\\bm\\b") ~ "m",
      #   TRUE ~ NA_character_
      # ),
      # 
      # WL_mm = case_when(
      #   unit == "mm" ~ numeric_value,
      #   unit == "m"  ~ numeric_value * 1000,
      #   TRUE ~ NA_real_
      # )
      # ) %>%
      # select(`ISO 8601 Time`, WL_mm)
      
    }, error = function(e) {
      message("❌ WL processing failed for ", basename(csv_path),
              " (device ", device_id, "): ", e$message)
      
      wl_raw %>%
        select(`ISO 8601 Time`) %>%
        mutate(WL_mm = NA_real_)
    })
    
  }
  
  
  # CURRENT data cleaning
  found_cols <- intersect(csv_columns, colnames(csv_df))
  missing_cols <- setdiff(csv_columns, colnames(csv_df))
  
  if (length(found_cols) == 0) {
    message("None of the requested columns found in ", basename(csv_path))
    next
  }
  
  if (length(missing_cols) > 0)
    message("Warning: missing columns in ", basename(csv_path), ": ",
            paste(missing_cols, collapse = ", "))
  
  extracted_df <- csv_df %>%
    select(all_of(found_cols)) %>%
    mutate(`ISO 8601 Time` = as.POSIXct(`ISO 8601 Time`, tz = "UTC"))
  
  # Merge WL
  extracted_df <- extracted_df %>%
    left_join(wl_df, by = "ISO 8601 Time") %>%
    mutate(WL_mm = as.numeric(WL_mm))
  
  # Add final columns
  extracted_df <- extracted_df %>%
    mutate(`Date & Time` = NA, .after = `ISO 8601 Time`) %>%
    mutate(
      `water level (mm)` = WL_mm,
      `Velocity (ft/s)` = NA,
      `Water Level (ft NAVD 88)` = NA,
      `Area (ft^2)` = NA,
      `Flow Rate (cfs)` = NA,
      .after = last_col()
    ) %>%
    select(-WL_mm)
  
  # Match or create Excel
  matched_excels <- match_excels_for_location(location_token, excel_files_all)
  
  if (length(matched_excels) == 0) {
    output_name <- paste0("Tilt Meter Flow Calculations ", location_token, ".xlsx")
    matched_excels <- file.path(excel_folder, output_name)
    message("Creating new workbook: ", basename(matched_excels))
  }
  
  # Append to Excel
  for (excel_path in matched_excels) {
    tryCatch(
      append_to_workbook(extracted_df, excel_path),
      error = function(e)
        message("Error appending to ", excel_path, ": ", e$message)
    )
  }
}
#THIS IS THE END TO SECTION ONE - DATA_TRANSFER_PROCESS


#THis line marks the complete conclusion of PT.1 of data processing use alt+ctrl+b to run everything above----------------------------------


#This is a gap for clarity :)


#Part.2 Excel output sheet priming and formatting ----------------------------------------------------------------------------------
#This section is to ensure that all worksheets in each workbook are created and match the standard.
#This is only necessary to run when a workbook is first created, but will not harm anything.


#library(openxlsx)

excel_folder <- "C:\\Users\\zasha\\OneDrive - East Carolina University\\3110 Lab @ ECU (Etheridge) - Mattamuskeet Source Tracking\\Data\\Analyses\\Flow\\"

# Find all Excel files that match your naming pattern
files <- list.files(
  path = excel_folder,
  pattern = "^Tilt Meter Flow Calculations .*\\.xlsx$",
  full.names = TRUE
)

# Define headers once
header_vel <- c(
  "Date and Time",
  "Tilt Velocity (cm/s)",
  "ADCP Velocity (ft/s)"
)

header_area <- c(
  "Date and Time",
  "Sensor WL (mm)",
  "Gauge WL (ft)",
  "Manual WL (ft NAVD88)"
)

header_wl <- c(
  "Date and Time",
  "Raw WL (ft)",
  "Corrected WL (ft)"
)

header_notes <- c(
  "Today's Date",
  "Data Date/Time",
  "Name",
  "Notes"
)

# Side blocks
Vel_block <- c(
  "Slope:",
  "Y-offset:",
  "R^2:"
)

WL_block <- c(
  "NAVD 88 Correction (ft):",
  "Slope:",
  "Y-offset:",
  "R^2:"
)

# Loop through files----

for (file_path in files) {
  
  message("Priming sheet: ", basename(file_path))
  
  wb <- loadWorkbook(file_path)
  
  # Add sheets (skip if they already exist)
  existing_sheets <- names(wb)
  
  if (!"Area_Calc" %in% existing_sheets) addWorksheet(wb, "Area_Calc")
  if (!"Vel_Corr" %in% existing_sheets) addWorksheet(wb, "Vel_Corr")
  if (!"WL_Corr" %in% existing_sheets) addWorksheet(wb, "WL_Corr")
  if (!"Notes" %in% existing_sheets) addWorksheet(wb, "Notes")
  
  # Rename sheet 1 → "Data" (only if not already named)
  if (existing_sheets[1] != "Data") {
    renameWorksheet(wb, 1, "Data")
  }
  
  # Write headers
  writeData(wb, "Vel_Corr", t(header_vel), startRow = 1, startCol = 1, colNames = FALSE)
  writeData(wb, "Area_Calc", t(header_area), startRow = 1, startCol = 1, colNames = FALSE)
  writeData(wb, "WL_Corr", t(header_wl), startRow = 1, startCol = 1, colNames = FALSE)
  writeData(wb, "Notes", t(header_notes), startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write side blocks
  writeData(wb, "Vel_Corr", Vel_block, startRow = 2, startCol = 5, colNames = FALSE)
  writeData(wb, "WL_Corr", WL_block, startRow = 2, startCol = 6, colNames = FALSE)
  
  # Save workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
}
#end

#This is the end of part.2 excel sheet priming/formatting----------------------------------------------


#This is a gap for clarity :)


#Part.3 Data Analysis Process---------------------------------------------------------------------------------------
#This code should be run after completely updating all workbooks with the most up-to-date data
#It essentially acts as a conversion from metric to imperial and a chronological sort
#The current iteration only modifies velocity and water level and sorts by date/time
#This section is completely safe
#This must be run after each sheet has been primed/formatted

# library(readxl)
# library(openxlsx)
library(dplyr)

# Folder containing all analysis Excel files
analysis_folder <- "C:\\Users\\zasha\\OneDrive - East Carolina University\\3110 Lab @ ECU (Etheridge) - Mattamuskeet Source Tracking\\Data\\Analyses\\Flow\\"

# List all .xlsx files in the folder
excel_files <- list.files(
  analysis_folder,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Loop through each file
for (file_path in excel_files) {
  
  message("Processing: ", basename(file_path))
  
  # CRITICAL FIX: look at ALL rows before guessing column types
  ana_df <- read_excel(
    file_path,
    sheet = "Data",
    guess_max = 31000
  )
  
  # Sort chronologically
  ana_df <- ana_df %>%
    mutate(`ISO 8601 Time` = as.POSIXct(`ISO 8601 Time`, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    arrange(`ISO 8601 Time`)
  
  # Compute velocity
  if ("Speed (cm/s)" %in% names(ana_df)) {
    ana_df$`Velocity (ft/s)` <- ana_df$`Speed (cm/s)` / 30.48
  }
  
  # Compute Water Level from mm to ft
  if ("water level (mm)" %in% names(ana_df)) {
    ana_df$`water level (ft)` <- ana_df$`water level (mm)` / 304.8
  }
  
  # Write back to workbook
  wb <- loadWorkbook(file_path)
  writeData(wb, sheet = "Data", ana_df)
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  message("Updated: ", basename(file_path))
}
#Final separate section for duplicate filtering
analysis_folder <- "C:\\Users\\zasha\\OneDrive - East Carolina University\\3110 Lab @ ECU (Etheridge) - Mattamuskeet Source Tracking\\Data\\Analyses\\Flow\\"

# List all .xlsx files in the folder
excel_files <- list.files(
  analysis_folder,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Loop through each file
for (file_path in excel_files) {
  
  message("Processing: ", basename(file_path))
  
  # Read the Data sheet
  Flow_data <- read_excel(
    file_path,
    sheet = "Data",
  )
  
  # Count duplicates
  dup_n <- sum(duplicated(Flow_data$`ISO 8601 Time`))
  
  if (dup_n > 0) {
    
    message("Duplicate timestamps found: ", dup_n, ". Removing duplicates.")
    
    # Remove, rewrite, rebuild, reorder
    Flow_data <- Flow_data[!duplicated(Flow_data$`ISO 8601 Time`), ]
    wb <- loadWorkbook(file_path)
    removeWorksheet(wb, "Data")
    addWorksheet(wb, "Data")
    writeData(wb, "Data", Flow_data)
    
    # Move Data (sheet 5) to the front
    worksheetOrder(wb) <- c(5, 1:4)
    
    saveWorkbook(wb, file_path, overwrite = TRUE)
    
    message("Updated: ", basename(file_path))
    
  } else {
    
    message("No duplicates found. Workbook unchanged.")
    
  }
}


#end

#This is the end of part.3 Data analysis----------------------------------------------------------------------


#This is a gap for clarity :)


#Part.4 Data appending *FOR APPLICABLE CANALS ONLY* --------------------

#This section, at the current moment in time, is only to be run for appending "Outfall_Bridge" to "Outfall", Using read_excel preserved date/time
#Use the shortcut ctrl+shift+c to quickly comment and uncomment

# library(readxl)

# 1. Define file paths
folderpath <- "C:\\Users\\zasha\\OneDrive - East Carolina University\\3110 Lab @ ECU (Etheridge) - Mattamuskeet Source Tracking\\Data\\Analyses\\Flow\\"

file1_path <- paste0(folderpath, "Tilt Meter Flow Calculations Outfall_Bridge.xlsx")
file2_path <- paste0(folderpath, "Tilt Meter Flow Calculations Outfall.xlsx")
output_path <- paste0(folderpath, "Tilt Meter Flow Calculations Outfall.xlsx")

# 2. Read Excel files WITHOUT altering column names
df1 <- read_excel(file1_path, sheet = 1, col_types = c("guess", "guess", rep("numeric", 7)))
df2 <- read_excel(file2_path, sheet = 1, col_types = c("guess", "guess", rep("numeric", 7)))
#
# # 3. Ensure ISO 8601 column stays as character (prevents Excel numeric conversion)
df1[[1]] <- as.character(df1[[1]])
df2[[1]] <- as.character(df2[[1]])
#
# # 4. Combine rows
combined_df <- rbind(df1, df2)
#
# # 5. Write combined file WITHOUT modifying headers
write.xlsx(combined_df, file = output_path, rowNames = FALSE)

file.remove(file1_path)

#This is the end of part.4 Data appending *FOR APPLICABLE CANALS*---------------------------------------------------------------------


#This is a gap for clarity :)



#Part.5 Data Quality Assurance and Quality Control (QA/QC) - REQUIRES USER INPUT EVERY ITERATION---------------
#This process is the most iterative and will be the largest time sink
#This section relies incredibly heavily on the plotly package
#YOU MUST CHANGE THE TARGET FILE EVERY SINGLE INSTANCE
#Add any potentially faulty data to the notes column of the excel sheet after running QA/QC
#cm/s and ft/s were mapped originally but reflect the same information with a different conversion factor.

#Library(openxlsx)
# library(dplyr)
# library(readxl)
library(plotly)

#Duplication checking loop
# Folder containing all analysis Excel files

flow_file <- "Outfall" #THIS NEEDS TO BE ADJUSTED EACH TIME

flow_folder <- "C:\\Users\\zasha\\OneDrive - East Carolina University\\3110 Lab @ ECU (Etheridge) - Mattamuskeet Source Tracking\\Data\\Analyses\\Flow\\Tilt Meter Flow Calculations "
flow_path <- paste(flow_folder,flow_file, ".xlsx", sep="")

# Read the Excel file, specific sheet
Flow_data <- read_excel(flow_path, sheet = "Data", col_types = c("guess", rep("numeric", 9)))

if ((sum(duplicated(Flow_data$`ISO 8601 Time`)))>0)
  stop("DUPLICATED TIME ERROR")

#add new variable to choose the specific columns (because they are not adjacent, read_excel(range = ) does not work)
selected_columns_Flow <- Flow_data %>%
  mutate(
    `ISO 8601 Time` = as.POSIXct(
      `ISO 8601 Time`,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC" ),
    `Speed (cm/s)`     = as.numeric(`Speed (cm/s)`),
    `water level (mm)` = as.numeric(`water level (mm)`),
    `Velocity (ft/s)`  = as.numeric(`Velocity (ft/s)`)
  )
#User input

# Inspect the column names (to make sure it is working)
#head(selected_columns_Flow) 

#Speed cm/s
# fig <- plot_ly(
#   data = selected_columns_Flow,
#   x = ~`ISO 8601 Time`,   # use backticks for names with spaces/special chars
#   y = ~`Speed (cm/s)`,                 # same here
#   type = 'scatter',
#   mode = 'lines+markers',
#   connectgaps = TRUE,
#   # If you have a categorical column for coloring, reference it here:
#   # color = ~CategoryColumn,
#   marker = list(size = 2, opacity = 1)
# ) %>%
#   layout(
#     title = "Speed (cm/s) over time",
#     xaxis = list(
#       type = 'date',
#       tickformat = "%m/%d/%Y %H:%M"
#     )
#          )
# fig

#WL in mm
fig <- plot_ly(
  data = selected_columns_Flow,
  x = ~`ISO 8601 Time`,   # use backticks for names with spaces/special chars
  y = ~`water level (mm)`,                 # same here
  type = 'scatter',
  mode = 'lines+markers',
  connectgaps = TRUE,
  # If you have a categorical column for coloring, reference it here:
  # color = ~CategoryColumn,
  marker = list(size = 3, opacity = 1)
) %>%
  layout(title = "Water level (mm) over time")

fig

summary(Flow_data$`water level (mm)`)
message("Ignored observations must match # of NA values in summary")

#speed, ft/s
fig <- plot_ly(
  data = selected_columns_Flow,
  x = ~`ISO 8601 Time`,   # use backticks for names with spaces/special chars
  y = ~`Velocity (ft/s)`,                 # same here
  type = 'scatter',
  mode = 'lines+markers',
  connectgaps = TRUE,
  # If you have a categorical column for coloring, reference it here:
  # color = ~CategoryColumn,
  marker = list(size = 2, opacity = 1)
) %>%
  layout(title = "Velocity (ft/s) over time")

fig

#This conlcudes PT.5-----

#All done :)





