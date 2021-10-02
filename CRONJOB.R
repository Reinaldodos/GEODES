setwd(dir = "~/R/GEODES/")
ssh_auth_sock <- system("echo $SSH_AUTH_SOCK", intern = TRUE)
Sys.setenv(SSH_AUTH_SOCK = ssh_auth_sock)

git2r::pull()

source(file = "The plan.R", echo = TRUE)
rmarkdown::render(input = "Description.Rmd", 
                  output_file = "Description.html")

git2r::add(path = "Description.html")
git2r::commit(message = "update HTML")
git2r::push()
