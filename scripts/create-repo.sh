
printf "Repository name: "
read name

curl -i -H 'Authorization: token '"$GITHUB_TOKEN"'' \
     -d '{ "name": "test" }' https://api.github.com/user/repos
