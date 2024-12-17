library(janitor)
library(tidyverse)
library(ggplot2)
library(knitr)
library(plyr)
library(dplyr)
library(readxl)

# Get the data into R----
# setwd("C:/Users/sethg/OneDrive/Documents/CoCo Lab/Lab_QC")
# setwd("C:/Users/swd22/OneDrive - USNH/Documents/GitHub/Lab_QC")
setwd("C:/Users/Seth/Documents/GitHub/Lab_QC")
# Enter the name of the result file. 
# csv_file <- "USGS-Ticks-qPCR01.xlsx"
csv_file <- "USGS-Ticks-qPCR03.csv"

# Use something like this to import from an excel file
#RawResults.dat <- as.data.frame(read_excel(csv_file, sheet = "Results", col_names = FALSE))
RawResults.dat <- read.csv(csv_file, header = FALSE)
PCR_Results.dat <- tail(RawResults.dat, n = -46)
PCR_Results.dat <- row_to_names(PCR_Results.dat, row_number = 1)
PCR_Results.dat <- clean_names(PCR_Results.dat)
PCR_Results.dat$ct <- as.numeric(PCR_Results.dat$ct)
PCR_Results.dat$quantity <- as.numeric(PCR_Results.dat$quantity)
PCR_Results.dat$`r_superscript_2` <- as.numeric(PCR_Results.dat$r_superscript_2)
PCR_Results.dat$ct <- ifelse(is.na(PCR_Results.dat$ct), 41, PCR_Results.dat$ct) # 41 in this command refers to cycle numbers. It should be greather than the number of cycles run on the assay

Samples <- unique(PCR_Results.dat$sample_name)
csv_base_name <- tools::file_path_sans_ext(basename(csv_file))

# Make a directory for the result files
dir.create(csv_base_name, showWarnings = FALSE)

# Generate QC report for the plate----
report_name <- csv_base_name
# Render the R Markdown file
rmarkdown::render("Tick Panel Plate QC.Rmd", 
                  params = list(report_name = report_name), 
                  output_file = paste0(csv_base_name, "/_", report_name, ".pdf"))

no_positive_samples <- PCR_Results.dat %>%
  group_by(sample_name) %>%
  dplyr::summarise(All_False = all(pass_check == FALSE)) %>%
  filter(All_False) %>%
  pull(sample_name)

# Remove the inconclusive sample IDs from the list of reports to generate
Samples <- setdiff(Samples, no_positive_samples)

# Loop through each sample name and generate a report----
for (sample in Samples) {
  # Define the name of the output dynamically for each sample
  report_name <- sample
  # Render the R Markdown file for each sample
  rmarkdown::render("Tick Report.Rmd", 
                    params = list(report_name = report_name), 
                    output_file = paste0(csv_base_name, "/", report_name, ".pdf"))
}
