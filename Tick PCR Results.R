# Load necessary libraries
library(janitor)
library(tidyverse)
library(ggplot2)
library(knitr)
library(plyr)
library(dplyr)
library(readxl)

# Set working directory to the folder containing the PCR result files
setwd("C:/Users/swd22/OneDrive - USNH/Documents/GitHub/Lab_QC/result-files")

# Get a list of all .xlsx files in the directory
file_list <- list.files(pattern = "*.xlsx")

# Loop through each file in the directory
for (csv_file in file_list) {
  
  # Import data from the "Results" sheet of the Excel file
  RawResults.dat <- read_excel(csv_file, sheet = "Results", col_names = FALSE)
  
  # Clean and prepare data
  PCR_Results.dat <- RawResults.dat %>%
    tail(n = -46) %>%  # Remove the first 46 rows
    row_to_names(row_number = 1) %>%  # Set the first row as column names
    clean_names() %>%  # Clean column names
    mutate(
      ct = as.numeric(ct),  # Convert ct column to numeric
      quantity = as.numeric(quantity),  # Convert quantity column to numeric
      r_superscript_2 = as.numeric(r_superscript_2),  # Convert r_superscript_2 column to numeric
      ct = ifelse(is.na(ct), 41, ct)  # Replace NA in ct with 41
    )
  
  # Get unique sample names
  Samples <- unique(PCR_Results.dat$sample_name)
  csv_base_name <- tools::file_path_sans_ext(basename(csv_file))
  
  # Create directory for result files within the result-files directory
  dir.create(csv_base_name, showWarnings = FALSE)
  
  # Generate QC report for the plate
  report_name <- csv_base_name
  rmarkdown::render(
    "C:/Users/swd22/OneDrive - USNH/Documents/GitHub/Lab_QC/Tick Panel Plate QC.Rmd", 
    params = list(report_name = report_name), 
    output_file = paste0(csv_base_name, "/_", report_name, ".pdf")
  )
  
  # Identify samples with no positive results
  no_positive_samples <- PCR_Results.dat %>%
    group_by(sample_name) %>%
    summarise(All_False = all(pass_check == FALSE)) %>%  # Check if all pass_check values are FALSE
    filter(All_False) %>%  # Filter samples with all FALSE pass_check
    pull(sample_name)  # Extract sample names
  
  # Remove inconclusive samples from the list
  Samples <- setdiff(Samples, no_positive_samples)
  
  # Loop through each sample name and generate a report
  for (sample in Samples) {
    report_name <- sample
    rmarkdown::render(
      "C:/Users/swd22/OneDrive - USNH/Documents/GitHub/Lab_QC/Tick Report.Rmd", 
      params = list(report_name = report_name), 
      output_file = paste0(csv_base_name, "/", report_name, ".pdf")
    )
  }
}
