#!/bin/bash

local="$HOME/config/scripts"

# Authorize device and store token
printf "Authorizing device with github.com\n"

printf "Enter your github username: "
read username

curl -u $username -d '{"scopes": ["repo", "user"], "note": "github cli"}' \
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



# Generate local ssh keys
#ssh-keygen
