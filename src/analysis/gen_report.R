#########################
###### Objective ########
#########################
# Input: Rmarkdown file
# Output: Pdf Report in gen directory


input_file <- "report.Rmd"

# Specify the output path including the filename
output_path <- "../../gen/report.pdf"

# Render the document
rmarkdown::render(input = input_file, output_file = output_path)
