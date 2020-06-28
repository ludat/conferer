# Conferer

![Hackage](https://img.shields.io/hackage/v/conferer)
![GitHub Workflow Status](https://img.shields.io/github/workflow/status/ludat/conferer/CI)
![Hackage-Deps](https://img.shields.io/hackage-deps/v/conferer)
![License](https://img.shields.io/github/license/ludat/conferer)

Most user oriented information you can find in the website: [conferer.ludat.io](https://conferer.ludat.io/)

## Testing and CI

We have about 50 tests, mostly for the conferer package since it's the one with most of the logic
both sources and FromConfig packages also have some tests but they are not tested as throughtly.

To run them do:

```shell
stack test # for everything
stack test conferer # to run only core library tests
stack test conferer-source-json # to run json source tests
stack test conferer-warp # to run warp fromConfig tests
```

Our ci is pretty intense as well we test with the last 5 versions of ghc (oldest being 8.0.2)
and with the nightly provided by stack and on every platform (windows, linux and macOS).

So I'm pretty confident that if someone introduces a platform dependent bug we'll catch it.

## Licensing

This library is released under the Mozilla Public License 2.0 which is a weak copyleft license.

As usual you can use this library for anything you want, the only difference is that **if you modify
the source and distribute** you must publish your modifications.

You can use it for privative software, GPL'd code, in house development and distribute
it as much as you like.

## Contributing

It's pretty simple, if you want to add a feature that already has a ticket just go ahead and create a PR
referencing the ticket.

If you want to add a new feature that doesn't have a ticket then you can add a ticket first to validate
that the feature makes sense and then create a PR (you can create the PR without the ticket if you feel
like it).

## The website

The website is based on [docusaurus](https://docusaurus.io/) so it's mostly generated from the
markdown stored in the [/docs](/docs) directory and built and deployed using
[netlify](https://www.netlify.com/).

## Publishing new versions

Publishing a new version is done using stack and parallel, beware that packages in the
examples directory shouldn't be published.

But sadly versions need to be bumped by hand following PVP.

So to actually publish a new version of packages:

```shell
find -name '*.cabal' | grep -v example | parallel --tty stack upload {//}
```

## Future maybe things

* Interpolate keys with other keys: `{a: "db", b: "${a}_thing"}`, getting `b`
  will give `"db_thing"` (maybe) even in different levels of configuration
* A LOT of sources
* A LOT of `FromConfig` implementations
