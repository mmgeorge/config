
name=$1

ssh_path=$(curl -H 'Authorization: token '"$GITHUB_TOKEN"'' \
                -d '{ "name": "'"$name"'" }' \
                https://api.github.com/user/repos \
                  | jq '.ssh_url')

echo "Created Github repository at ${ssh_path//\"}"

git clone "${ssh_path//\"}"
