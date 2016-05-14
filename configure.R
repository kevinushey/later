library(rmarkdown)
output_format <- github_document(html_preview = FALSE)
render("README.Rmd", output_format = output_format)
