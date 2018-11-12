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
- [x] Use [lens](http://hackage.haskell.org/package/lens)
- [x] Apply ReaderT Config on Servant handlers (Read [here](https://haskell-servant.readthedocs.io/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html))
- [x] Observe [this article](http://www.parsonsmatt.org/2016/07/08/servant-persistent_updated.html)
- [x] Migrate to postgreSQL (Read [here](https://www.yesodweb.com/book/persistent#persistent_something_besides_sqlite))
- [x] Use docker for postgresSQL
- [x] Write down how to get started in [ScrapBox](https://scrapbox.io/haskell-twitter-server/)
- [x] Add logging using RIO (Read [here](https://hackage.haskell.org/package/rio-0.0.3.0/docs/RIO-Prelude-Logger.html))
- [ ] Implement CLI using attoparsec (Read [here](https://github.com/bos/attoparsec) [example](https://github.com/input-output-hk/log-classifier/blob/develop/src/CLI.hs))
- [ ] Lookup Twitter API (Read [here](https://developer.twitter.com/))
- [ ] Follow/Unfollow an User
- [ ] Fetch mentioned tweets
- [ ] Timeline API
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

testing git