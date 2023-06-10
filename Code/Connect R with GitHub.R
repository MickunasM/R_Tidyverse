library(usethis)

use_git_config(user.name = "MickunasM", #your github user name
               user.email = "m.mickunas@outlook.com") # the email you used when you signed upo with github

# In the RStudio console type the following to generate the PAT 
usethis::create_github_token()

# Store the PAT explicitly 
gitcreds::gitcreds_set()
