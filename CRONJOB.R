setwd(dir = "~/R/GEODES/")
ssh_auth_sock <- system("echo $SSH_AUTH_SOCK", intern = TRUE)
Sys.setenv(SSH_AUTH_SOCK = ssh_auth_sock)

repo = git2r::repository()
SSH = git2r::cred_ssh_key()

git2r::pull(repo = repo, credentials = SSH)

source(file = "The plan.R", echo = TRUE)
rmarkdown::render(input = "Description.Rmd", 
                  output_file = "Description.html")

git2r::add(repo = repo, path = "Description.html")
git2r::commit(repo = repo, message = "update HTML")
git2r::push(credentials = SSH, object = repo)
