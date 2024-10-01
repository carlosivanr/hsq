library(here)
library(quarto)

# Set file_in to the copied and renamed clinic-specific .qmd file. Serves as
# an input to the quarto_render() function
file_in <- here("scripts/primary_outcomes_completion_report/primary_outcomes_completion_report.qmd")

# Render the report
quarto_render(
  input = file_in
  )
