# infinite-trivia

A simple mobile client for infinitely many [OpenTDB.com][openTDB] trivia questions.

[Play the game](https://rix0rrr.github.io/infinite-trivia/)

## Development
The development of this client relies on the following tools

* node
* elm, elm-format, elm-live

### Node
See the [node installation guide][node-install] on how to install `node`. Once installed used  the following command to install the rest of the tools.

```sh
npm install
```

### Scripts
There are some scripts that automate various things. Take a look at the `package.json` for an up to date list.

#### `npm run make`
Creates all the artifacts for the published app. Don't forget to commit any changes.

#### `npm run live`
Runs a local development server.

[node-install]: https://nodejs.org/en/download/
[openTDB]: https://opentdb.com/