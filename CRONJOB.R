setwd(dir = "~/R/GEODES/")
ssh_auth_sock <- system("echo $SSH_AUTH_SOCK", intern = TRUE)
Sys.setenv(SSH_AUTH_SOCK = ssh_auth_sock)
Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/bin/pandoc")

pacman::p_load(git2r)
pacman::p_load(rmarkdown)
repo = git2r::repository()
SSH = git2r::cred_ssh_key()

git2r::pull(repo = repo, credentials = SSH)

source(file = "The plan.R", echo = TRUE)
drake::clean(list = c("data", "output_GEODES"))
drake::make(plan = the_plan)

rmarkdown::render(input = "Description.Rmd",
                  output_file = "Description.html")

git2r::add(repo = repo, path = "Description.html")
git2r::commit(repo = repo, message = "update HTML")
git2r::push(credentials = SSH, object = repo)

