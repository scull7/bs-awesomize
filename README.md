[![Build Status](https://www.travis-ci.org/scull7/bs-awesomize.svg?branch=master)](https://www.travis-ci.org/scull7/bs-awesomize)
[![Coverage Status](https://coveralls.io/repos/github/scull7/bs-awesomize/badge.svg?branch=master)](https://coveralls.io/github/scull7/bs-awesomize?branch=master)

# bs-awesomize
A ReasonML implementation of the Awesomize library for data validation / scrubbing

# Why?
We use Awesomize to ensure that data which comes into our system
fits the proper shape and is within expected constraints.  There are several
other libraries which aim to handle this, however, translating them to
ReasonML proved problematic.  Also, the Awesomize library is asynchronous by
default which allows much greater flexibility in validation / scrubbing code.

# Status

This library can be considered production ready.

# Installation

Inside of a BuckleScript project:
```sh
yarn add bs-awesomize
```

Then add `bs-awesomize` to your `bs-dependencies` in your `bsconfig.json`
```json
{
  "bs-dependencies": [ "bs-awesomize" ]
}
```

# Usage

In order to use awesomize you will want to provide a "schema" which
consists of a map of `fields` to `field definitions`.

## Field definition
```reason
type maybe = option(Js.Json.t);
type jsonMap = Belt.Map.String.t(maybe);

type definition = {
  read: Js.Dict.t(Js.Json.t) => Js.Promise.t(maybe),
  sanitize: option((maybe, jsonMap) => Js.Promise.t(maybe)),
  validate: list((maybe, jsonMap) => Js.Promise.t(option(string))),
  normalize: option((maybe, jsonMap) => Js.Promise.t(maybe)),
};
```
