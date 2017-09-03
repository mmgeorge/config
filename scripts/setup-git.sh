
printf "Git username: "
read username
printf "Git email"
read email

git config --global core.editor emacs
git config --global user.name "Matt"
git config --global user.email matt@mattgeorge.net
