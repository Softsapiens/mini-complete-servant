# mini-complete-servant

A mini but complete example of API construction with persistence plus authorization features with servant framework.







# create a new user
$ curl --verbose --request POST --header "Content-Type: application/json" \
    --data '{"ident": 1, "firstName": "Albert", "lastName": "Einstein", "email": "albert@mit.edu"}' \
	 http://localhost:8081/users

# get all users in database
$ curl --verbose --request GET --header "Content-Type: application/json" \
 	http://localhost:8081/users

# get certain user in database
$ curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8081/users/albert@mit.edu
