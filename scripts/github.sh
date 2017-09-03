#!/bin/bash

local="$HOME/config/scripts"

# Authorize device and store token
printf "Authorizing device ($HOSTNAME) with github.com\n"

printf "Enter your github username: "
read username
addusername="export GITHUB_USERNAME=$username"

curl -u $username -d '{"scopes": ["repo", "user", "admin:public_key"], "note": "'"$HOSTNAME"' Access"}' \
     https://api.github.com/authorizations > $local/auth.json

token=$(jq '.token' $local/auth.json)
echo "Storing token: $token"

printf "Adding github token to enviornment variables  ... "
addtoken="export GITHUB_TOKEN=$token"

if grep -Fxq "$addtoken" $HOME/.bashrc
then
    printf "SKIPPING\n"
else
    echo "$addtoken" >> $HOME/.bashrc
    printf "ADDED!\n"
fi

if grep -Fxq "$addusername" $HOME/.bashrc
then
    printf "SKIPPING\n"
else
    echo "$addusername" >> $HOME/.bashrc
    printf "ADDED!\n"
fi

source $HOME/.bashrc

# Generate & add local keys
ssh-keygen
key=$(cat $HOME/.ssh/id_rsa.pub)

curl -i -H 'Authorization: token '"$token"'' \
     -d '{ "title": "'"$HOSTNAME"'", "key": "'"$key"'" }' https://api.github.com/user/keys \
     > $local/key.json

