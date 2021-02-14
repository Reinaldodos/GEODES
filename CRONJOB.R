git2r::pull()

source(file = "The plan.R")
rmarkdown::render(input = "Description.Rmd", 
                  output_file = "Description.html")

git2r::add(path = "Description.html")
git2r::commit(message = "update HTML")
git2r::push()
