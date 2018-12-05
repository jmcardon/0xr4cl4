# 3311 Project Oracle Diff tool

This tool scans all files beginning with `at` and ending with `txt` to run through the correct implementation
given by Prof. Jackie Wang, along with your eiffel executable, and compares their differences.

It does not use any `atX.expected.txt` files. Rather, it runs them through the oracle and holds the differences in memory, which is a lot faster than saving onto
a temporary location and reading off disk.

The diff tool is tolerant of leading and trailing whitespace, but not of inter-word whitespace. I could change this,
but why on earth would you have different spaces between words when you can literally copy the error message from the lab spec!!!

### Usage
Run by setting:
* `--eiffel-path` to the path of your eiffel executable (the file itself. Typically the location inside EIFGENs).
*`--oracle-path` to the path of the oracle provided by the professor (just `oracle` is fine, since I am also including it here)
* `--at-file-dir` to the path of where your `atX.txt` files are.

example usage:

See `runOracle` for an example.

example output that is successful:

![All Ok](allOk.png)

example output with errors:

![Not Ok](errors.png)