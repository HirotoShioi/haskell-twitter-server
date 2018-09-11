# twitter-server in Haskell

Twitter server written in Haskell. This is for studying purposes.

## TODO

- [x] Define Config datatype
- [x] Define generator for initial db
- [x] Define User datatype and utilize it
- [ ] Validation (Username length, tweet length, etc)
- [ ] Enforce UserName to only use latin-charcters (How?)
- [ ] Use [lens](http://hackage.haskell.org/package/lens)
- [ ] Apply ReaderT Config on Servant handlers (Read [here](https://haskell-servant.readthedocs.io/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html))
- [ ] Lookup Twitter API (Read [here](https://developer.twitter.com/))
- [ ] Follow/Unfollow
- [ ] Timeline API
- [ ] Migrate to postgreSQL (Read [here](https://www.yesodweb.com/book/persistent#persistent_something_besides_sqlite))
- [ ] Create frontend with [elm](https://elm-lang.org/)
- [ ] User docker for postgresSQL
- [ ] Authentication (Read [here](https://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html))
- [x] Overall more data (User has only username)
- [ ] Favorites
- [ ] Retweets
- [ ] Test cases
- [ ] More endpoints!
- [ ] Search API
- [ ] Pagination API