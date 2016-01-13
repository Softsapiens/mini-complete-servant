## Description

A mini but complete example of API construction with persistence plus authorization features with servant framework.

This project is based on:

- https://github.com/haskell-servant/servant/tree/master/servant-examples/auth-combinator
- https://github.com/parsonsmatt/servant-persistent
- https://github.com/haskell-servant/HaskellSGMeetup2015/blob/master/examples/authentication-combinator/AuthenticationCombinator.hs

## The API:

- GET `/users` returns a list of all users in the database
- GET `/users/:email` returns the first user whose name is `:email`, and returns 404 if the user doesn't show up.
- POST `/users` with JSON like `{ "ident": "Int", "firstName": "String", "lastName": "String", "email": "String" }` to create a User.

## How to play it

### Installation

Using [stack](https://github.com/commercialhaskell/stack):

1. Clone this repo
2. `stack setup`
3. `stack build`
4. `stack exec mini-complete-servant-exe`


### Database configuration:

You are going to need PostgreSQL installed and listening on port 5432. The default configuration uses a database name `mini-complete-servant-db` with username/password test:test.

#### OSX

Install the package `brew info postgresql`

Start the engine `postgres -D /usr/local/var/postgres`

Create our test user
`createuser -h localhost -p 5432 -s test`

Create the database instance
`createdb mini-complete-servant-db -U test`

Check the installation
`psql -d mini-complete-servant-db -U test`

### Tests

#### create a new user
$ curl --verbose --request POST --header "Content-Type: application/json" \
    --data '{"ident": 1, "firstName": "Albert", "lastName": "Einstein", "email": "albert@mit.edu"}' \
	 http://localhost:8081/users

#### get all users in database
$ curl --verbose --request GET --header "Content-Type: application/json" \
 	http://localhost:8081/users

#### get certain user in database
$ curl --verbose --request GET --header "Content-Type: application/json" \
  http://localhost:8081/users/albert@mit.edu
