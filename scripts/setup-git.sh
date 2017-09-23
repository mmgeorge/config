
printf "Git username: "
read USERNAME
printf "Git email: "
read EMAIL

git config --global core.editor emacs
git config --global user.name $USERNAME
git config --global user.email $EMAIL
