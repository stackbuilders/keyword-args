[![Build Status](https://travis-ci.org/stackbuilders/keyword-args.svg)](https://travis-ci.org/stackbuilders/keyword-args)

# Keyword Args

Parses a configuration file with keywords followed by arguments,
separated by a space. An example of a file in this format is the
sshd_config file.

The parser in this library strips out comments, and normalizes
whitespace. It includes a binary, `keyword-args`, which reads from
STDIN and emits a CSV file with two columns, containing the keyword
followed by the values.

# Author

Justin Leitgeb

# Copyright

(C) 2015 Stack Builders Inc.
