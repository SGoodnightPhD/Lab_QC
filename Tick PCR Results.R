library(janitor)
library(tidyverse)
library(ggplot2)
library(knitr)
library(plyr)
library(dplyr)

#setwd("C:/Users/sethg/OneDrive/Documents/CoCo Lab/Lab_QC")
setwd("C:/Users/swd22/OneDrive - USNH/Documents/GitHub/Lab_QC")

csv_file <- "USGS-Ticks-qPCR01-altered.csv"

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

##### Place this in a markdown file that saves with the name of the qPCR plate
# Check for at least one positive for each target
# Each target has an empirically determined Ct cutoff that should be changed to reflect the assay as they are updated
PCR_Results.dat <- PCR_Results.dat %>%
  mutate(
    pass_check = case_when(
      target_name == "A. phagocytophilum" & ct < 35 & amp_score > 1 & cq_conf > 0.8 ~ TRUE,
      target_name == "B. microti" & ct < 35 & amp_score > 1 & cq_conf > 0.8 ~ TRUE,
      target_name == "B. burgdorferi" & ct < 34 & amp_score > 1 & cq_conf > 0.8 ~ TRUE,      
      target_name == "B. mayonii" & ct < 35 & amp_score > 1 & cq_conf > 0.8 ~ TRUE,
      target_name == "B. miyamotoi" & ct < 36 & amp_score > 1 & cq_conf > 0.8 ~ TRUE,
      target_name == "E. muris" & ct < 34 & amp_score > 1 & cq_conf > 0.8 ~ TRUE,      
      target_name == "Powassan virus" & ct < 35 & amp_score > 1 & cq_conf > 0.8 ~ TRUE,
      target_name == "Extraction Control" & ct < 36 & amp_score > 1 & cq_conf > 0.8 ~ TRUE,
      TRUE ~ FALSE
    )
  )

Anaplasma_check <- PCR_Results.dat[PCR_Results.dat$target_name == "A. phagocytophilum", ]
Ana_pass <- any(Anaplasma_check$pass_check)

Babesia_check <- PCR_Results.dat[PCR_Results.dat$target_name == "B. microti", ]
Babesia_pass <- any(Babesia_check$pass_check)

Bburgdorf_check <- PCR_Results.dat[PCR_Results.dat$target_name == "B. burgdorferi", ]
Bburgdorf_pass <- any(Bburgdorf_check$pass_check)

Bmayo_check <- PCR_Results.dat[PCR_Results.dat$target_name == "B. mayonii", ]
Bmayo_pass <- any(Bmayo_check$pass_check)

Bmiya_check <- PCR_Results.dat[PCR_Results.dat$target_name == "B. miyamotoi", ]
Bmiya_pass <- any(Bmiya_check$pass_check)

Emuris_check <- PCR_Results.dat[PCR_Results.dat$target_name == "E. muris", ]
Emuris_pass <- any(Emuris_check$pass_check)

Powassan_check <- PCR_Results.dat[PCR_Results.dat$target_name == "Powassan virus", ]
Powassan_pass <- any(Powassan_check$pass_check)

Extraction_check <- PCR_Results.dat[PCR_Results.dat$target_name == "Extraction Control", ]
Extraction_pass <- any(Extraction_check$pass_check)


#Inconclusive Sample Check
no_positive_samples <- PCR_Results.dat %>%
  group_by(`sample_name`) %>%
  summarise(All_False = all(pass_check == FALSE)) %>%
  filter(All_False) %>%
  pull(`sample_name`)
no_positive_samples <- as.data.frame(no_positive_samples)

