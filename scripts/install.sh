sudo apt-get install curl jq git -y
git config --system init.defaultbranch main

startup="$HOME/config/scripts/startup.sh"
eload="$HOME/config/lisp/load.el"


printf "Checking whether to add startup.sh to .bashrc  ... "
if grep -Fxq "source $startup" $HOME/.bashrc
then
    printf "SKIPPING\n"
else
    echo "source $startup" >> $HOME/.bashrc
    printf "ADDED!\n"
fi


printf "Checking whether to add emacs configuration load path to .emacs ... "
if grep -Fxq "(load \"$eload\")" $HOME/.emacs
then
    printf "SKIPPING\n"
else
    echo "(setq EMACS-LOAD-PATH \"$HOME/config/lisp/\")" >> $HOME/.emacs
    echo "(load \"$eload\")" >> $HOME/.emacs
    printf "ADDED!\n"
fi

printf "Installing tmux conf into home directory ... "
cp ../tmux.conf  $HOME/.tmux.conf
printf "ADDED!\n"
