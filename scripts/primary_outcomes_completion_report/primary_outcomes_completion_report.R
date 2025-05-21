# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez PhD. CU Anschutz Dept. of Family Medicine
# 10-07-2024

# Primary outcomes completion report
# This R script is designed to render a Quarto document for the primary outcomes
# completion report and then place the newly rendered file in a shared Egnyte
# folder for the HSQ project.

# This file is meant to be launched via a .bat file to automate its execution on
# regular basis.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(here)
library(quarto)
library(stringr)

# Set file_in to the copied and renamed clinic-specific .qmd file. Serves as
# an input to the quarto_render() function
file_in <- here(
  "scripts/primary_outcomes_completion_report/primary_outcomes_completion_report.qmd"
)

# Render the report
quarto_render(
  input = file_in
)

# Place a copy of the most recent report in the Egnyte folder -----------------
from <- here(
  "scripts/primary_outcomes_completion_report/primary_outcomes_completion_report.html"
)

print(getwd())

print(from)

to <- str_c(
  "Z:/Shared/DFM/HSQ_Shared/Reporting/Primary Outcome Completion Report/report_", 
  Sys.Date(),
  ".html"
)

print(to)

file.exists(to)

# The names of the arguments and the paths are the same. Needs from = from and
# to = to to make it explicity that the keyword is set to the input path.
file.copy(from = from, to = to, overwrite = TRUE)