library(janitor)
library(tidyverse)
library(ggplot2)
library(knitr)

csv_file <- "2024-05-15 Tick Pathogen PCR Test 6(Results).csv"

RawResults.dat <- read.csv(csv_file, header = FALSE)
PCR_Results.dat <- tail(RawResults.dat, n = -46)
PCR_Results.dat <- row_to_names(PCR_Results.dat, row_number = 1)
PCR_Results.dat <- clean_names(PCR_Results.dat)
PCR_Results.dat$ct <- as.numeric(PCR_Results.dat$ct)
PCR_Results.dat$quantity <- as.numeric(PCR_Results.dat$quantity)
PCR_Results.dat$`r_superscript_2` <- as.numeric(PCR_Results.dat$r_superscript_2)
PCR_Results.dat$ct <- ifelse(is.na(PCR_Results.dat$ct), 41, PCR_Results.dat$ct)

Samples <- unique(PCR_Results.dat$sample_name)
csv_base_name <- tools::file_path_sans_ext(basename(csv_file))

#setwd("C:/Users/sethg/OneDrive/Documents/CoCo Lab/Lab_QC")
setwd("C:/Users/swd22/OneDrive - USNH/Documents/GitHub/Lab_QC")
dir.create(csv_base_name, showWarnings = FALSE)

# Loop through each sample name and generate a report
for (sample in Samples) {
  # Define the name of the output dynamically for each sample
  report_name <- sample
  # Render the R Markdown file for each sample
  rmarkdown::render("Tick Report.Rmd", 
                    params = list(report_name = report_name), 
                    output_file = paste0(csv_base_name, "/", report_name, ".pdf"))
}
