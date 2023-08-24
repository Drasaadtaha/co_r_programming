
#.........................................................................................
# The vignettes of the website  https://github.com/amices/mice#vignettes
# are not available as .Rmd thus I have to convert the .html to an .md and then to an .Rmd
#.........................................................................................

md_file = "/R_programming_sessions/missing_values_outliers/Rmd_files/vignette_1/vignette_1.md"
rmd_file = gsub(pattern = '.md$', replacement = '.Rmd', x = md_file)

rmarkdown::pandoc_convert(input = "/R_programming_sessions/missing_values_outliers/Rmd_files/vignette_1/Ad hoc methods and mice.html", 
                          output = md_file)

rmarkdown::render(input = md_file, 
                  output_format = "md_document",
                  output_file = rmd_file)

# knitr::purl(rmd_file, output = gsub(pattern = '.Rmd$', replacement = '.R', x = rmd_file))
