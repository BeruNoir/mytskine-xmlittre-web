# Installing

## Stack

If Stack is not installed,
follow the [upstream instructions](http://docs.haskellstack.org/en/stable/install_and_upgrade/).


## External dependencies

```
sudo apt install sqlite3 build-essential pkg-config zlib1g-dev libmariadbd-dev libsqlite3-dev libpcre3-dev libicu-dev
```


## Build

Install the basic tools, adding `--install-ghc` if you don't want to use the system's ghc.
Then build the package.

```
stack install yesod-bin cabal-install
stack build
```


