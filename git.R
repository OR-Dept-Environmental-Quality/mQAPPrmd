## Rename a branch

cd <branch_directory>
  git checkout <old_name>
  git push origin -u <new_name>
  git push origin -d <old_name>
  
  ## update a branch from Master
  git checkout <branch_name>
  # first commit your branch changes
  git commit -m <commit_message>
  git push origin <branch_name>
  
  # merge changes from master
  git merge origin/master

# if there are conflicts, fix the conflicts and then commit the result into the branch
# then repeat git merge origin master
