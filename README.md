# twitter-server in Haskell

Twitter server written in Haskell. This is for studying purposes.

## TODO

- [x] Define Config datatype
- [x] Define generator for initial db
- [ ] Create User datatype
- [ ] Validation (Username length, tweet length, etc)
- [ ] Lookup Twitter API (Read [here](https://developer.twitter.com/))
- [ ] Follow/Unfollow
- [ ] Timeline API
- [ ] Use [lens](http://hackage.haskell.org/package/lens)
- [ ] Migrate to postgreSQL (Read [here](https://www.yesodweb.com/book/persistent#persistent_something_besides_sqlite))
- [ ] Create frontend with [elm](https://elm-lang.org/)
- [ ] User docker for postgresSQL
- [ ] Authentication (Read [here](https://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html))
- [ ] Overall more data (User has only username)
- [ ] Favorite
- [ ] Test cases
- [ ] More endpoints!
- [ ] Search API
- [ ] Pagination API