[![Build Status](https://travis-ci.org/stackbuilders/keyword-args.svg)](https://travis-ci.org/stackbuilders/keyword-args)

# Keyword Args

Parses a configuration file with keywords followed by arguments,
separated by a space. An example of a file in this format is the
sshd_config file.

The parser in this library strips out comments, and normalizes
whitespace. It includes a binary, `keyword-args`, which reads from
STDIN and emits a CSV file with two columns, containing the keyword
followed by the values.

## Executable Usage

Send input to STDIN, and the output will be a CSV with comments stripped and with unnecessary whitespace removed.

```bash
$ printf "#Testing\nkeyword a bunch of values #comment" | ./dist/build/keyword-args/keyword-args
keyword,"a bunch of values"
```

## Library Usage

This project exposes a library that is usable from Haskell programs for extracting data from input in Keyword-Argument format.

## Author

Justin Leitgeb

## License

MIT

## Copyright

(C) 2015 Stack Builders Inc.
