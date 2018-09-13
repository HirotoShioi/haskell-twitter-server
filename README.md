# twitter-server in Haskell

Twitter server written in Haskell. This is for studying purposes.

## TODO

- [x] Define Config datatype
- [x] Define generator for initial db
- [x] Define User datatype and utilize it
- [x] Validation (Username length, tweet length, etc)
- [x] Enforce UserName to only use latin-charcters
- [x] Overall more data (User has only username)
- [x] Follow tweet structure by mentions
- [x] Sort tweet appropriately
- [ ] Fetch mentioned tweets
- [ ] Use [lens](http://hackage.haskell.org/package/lens)
- [ ] Apply ReaderT Config on Servant handlers (Read [here](https://haskell-servant.readthedocs.io/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html))
- [ ] Lookup Twitter API (Read [here](https://developer.twitter.com/))
- [ ] Follow/Unfollow an User
- [ ] Timeline API
- [ ] Migrate to postgreSQL (Read [here](https://www.yesodweb.com/book/persistent#persistent_something_besides_sqlite))
- [ ] User docker for postgresSQL
- [ ] Authentication (Read [here](https://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html))
- [ ] Favorites
- [ ] Retweets
- [ ] Test cases
- [ ] Search API
- [ ] Pagination API

### Always
- [ ] More endpoints!

### Need help
- [ ] Create frontend with [elm](https://elm-lang.org/) or [purescript](http://www.purescript.org/)