# Netrunner 1996
This is a fork of the netrunner client used for jinteki.net, with the goal of porting over all the features, functionality, and cards of 1996 netrunner.

## Implementation status:
* 573 / 584 cards implemented so far
* 232 of those with unit tests
* The ONR trace mechanic has been implemented, with all the quirks (cancelling, cost multiplication, base link cards, post-trace link adjustment, max strength manipulation, secret bidding, bonus link for run, hacker tracker central confusion, ldl analyzer multiplication, I even gave non-onr runners the link value on the card, etc)
* Systems developed to allow for the unique virus mechanics (counters have effects outside of their programs)
* Systems developed for the unique counters corps can place on runners (baskervilles, etc)
* Two special ID's have been created, which some mechanics rely on (a small handful of the viruses, ONR style purging, the bad-pub win condition will be on the runner side)
* Most ONR cards that refer to trace do not interact with ANR cards that refer to trace (the ones which care about success/non-success and the credit providing ones mostly do though). Other than that, almost all cards should be (mostly) interoperable

This little graph is a visual representation of the implementation status so far. Green for tests, orange for no tests, and red for "I'm not implementing that (yet)"

## Interop
The ONR Traces and the ANR traces have different mechanics, so most of their bespoke interaction cards don't work together.

Two specific virus programs (Crumble and the other one for HQ. Cascade?) rely on the runner being the fake identity "ONR Braniac: Espionage Enjoyer".

The bad publicity win condition depends on the runner being that same fake identity.

Barring EXACTLY those interactions, these cards should be 100%(tm) interoperable.

## Faithfulness
These cards are mostly as I interpreted them. There is no way to get rulings on most of these cards, and some of them are ambigious. Additionally, I have implemented them as if they existed in the same engine as android: netrunner, and thus they are using that timing structure. Arasaka.de has some ruling, but a scant few, unfortunately, and I don't know anything about how reliable they are.

If there's something that's obviously wrong, let me know!

## Cards that aren't implemented
The following cards are not implemented at all for some reason or another. Other than that, everything should work.

These cards are playable, but manual
* Elena Laskova (automating this is too hard, but it's easy to do manually)
* Government Contract (There's no systems to track these credits, it's possible to do manually)
* Twenty-Four-Hour Surviellance (automating this is cumbersome, but it's easy to do manually)
 
These cards should not be played
* Code Viral Cache (I haven't done any logic for clicking on any of the bespoke counters)
* Disinfectant, Inc (I haven't introduced any timing for avoiding virus counters - it would require a significant engine rewrite)
* Fait Accompli (there are no systems for placing counters on servers)
* Incubator (see above)
* I Spy (see above)
* Mercenary Subcontract (the timing does not line up in any meaningful way)
* Pox (see above)
* Tutor (I couldn't figure it out)
  
![1996 Implementation Graph](graph-implementation.jpg)

## TODO
* Make an ONR legal format
* card art for the special id's (even if it's bad)
* seeded (in terms of repeatable rng) draft/sealed generator to allow for an easily accessible sealed format
* describe-counters command that lets you see the types and quantities of counters on a card (there are lots of them...)
* explain-counter command that explains what a counter does (what does a militech counter do? what about an I-Spy counter?)
* Ephemeral counters that live in servers

# Netrunner in the browser

Hosted at [http://www.jinteki.net](http://www.jinteki.net). [Example of gameplay](https://www.youtube.com/watch?v=cnWudnpeY2c).

![screenshot](http://i.imgur.com/xkxOMHc.jpg)

## Card implementation status

[Card rules implementation status](https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml)

## Development

### Quickstart

*(There's a [docker](#using-docker) section down there!)*

Install [Leiningen](https://leiningen.org/),
[NodeJS](https://nodejs.org/en/download/package-manager/#macos) and
[MongoDB](https://docs.mongodb.com/manual/installation/).

This project runs on Java 8. If you're on OSX or Linux, we recommend using
[jenv](https://github.com/jenv/jenv/blob/master/README.md) to manage your java environment.

You can check your setup by running:

    $ lein version
    Leiningen 2.9.6 on Java 16.0.1 OpenJDK 64-Bit Server VM

Your exact version numbers below may vary, but we require Java 1.8+.

Populate the database and create indexes using:

    $ lein fetch [--no-card-images]
    1648 cards imported

    $ lein create-indexes
    Indexes successfully created.

You can optionally pass `--no-card-images` if you don't want to download images from
[NetrunnerDB](https://netrunnerdb.com/), as this takes a while. See `lein fetch help`
for further options.

To install frontend dependencies, run:

    $ npm ci
    added 124 packages, and audited 125 packages in 2s

To compile CSS:

    $ npm run css:build
    compiled resources/public/css/netrunner.css

Optionally you can say `npm run watch:css` to watch for changes and automatically
recompile.

Compile ClojureScript frontend:

    $ npm run cljs:build
    [:app] Compiling ...
    [:app] Build completed. (238 files, 0 compiled, 0 warnings, 22.18s)

Finally, launch the webserver and the Clojure REPL:

    $ lein repl
    dev.user=>

and open [http://localhost:1042/](http://localhost:1042/).

### Using Docker

You'll need to install [Docker](https://docs.docker.com/get-docker/) and [Docker-Compose](https://docs.docker.com/compose/install/). After that, just run `$ docker-compose up --build` in the project directory (or do the GUI-equivalent of this). If this fails because it "couldn't fetch dependencies", try again, it was just a networking error.

It can take a while. You'll see lots of messages, so just wait until you see something like `netrunner-server-1 | nREPL server started on port 44867`. After that, you can visit [http://localhost:1042/](http://localhost:1042/) and the server should be running.

While coding clojure it's important to have a REPL connection going. The server's REPL is configured to always run on port `44867`, so you can connect using `$ lein repl :connect nrepl://localhost:44867` (or the program of your preference, like your code editor).

Now, let's populate the database and create indexes. First, let's open a terminal inside the server container: `$ docker exec -it netrunner-server-1 /bin/bash`. Now, inside this new therminal, we'll run these two commands: *The `--no-card-images` is optional, and removing it causes the card images to be downloaded, which can be slower.*

```
 $ lein fetch --no-card-images
    1648 cards imported

 $ lein create-indexes
    Indexes successfully created.
```

After this, just restart the server by running `(restart)` in the REPL.

To do testing, you run them inside the container: `$ docker exec -it netrunner-server-1 /bin/bash` and then `$ lein kaocha`.
### Tests

To run all tests:

    $ lein kaocha
    Ran 2640 tests containing 44704 assertions.
    0 failures, 0 errors.

To run a single test file:

    $ lein kaocha --focus game.cards.agendas-test
    Ran 216 tests containing 3536 assertions.
    0 failures, 0 errors.

Or a single test:

    $ lein kaocha --focus game.cards.agendas-test/fifteen-minutes
    Ran 1 tests containing 29 assertions.
    0 failures, 0 errors.

For more information refer to the [development guide](https://github.com/mtgred/netrunner/wiki/Getting-Started-with-Development).

### Further reading

- [Development Tips and Tricks](https://github.com/mtgred/netrunner/wiki/Development-Tips-and-Tricks)
- [Writing Tests](https://github.com/mtgred/netrunner/wiki/Tests)
- "Profiling Database Queries" in `DEVELOPMENT.md`

## License

Jinteki.net is released under the [MIT License](http://www.opensource.org/licenses/MIT).
