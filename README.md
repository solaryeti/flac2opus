# Flac2opus
## Synopsis

Flac2opus is a command line application to encode flac files to opus. While flac2opus is mainly just a helper for running `opusenc`, it adds a progress bar and the ability to run with parallel workers.

## Installing

Flac2opus is written in Haskell and can therefore most easily be built using stack. To do this obtain the latest release from https://github.com/commercialhaskell/stack/releases and build/install with
```bash
stack build --copy-bins
```

## Usage Example

Help can be obtained by running `flac2opus --help`.

flac2opus is run by specifying the source directory, which contain the flac files to be encoded, and the destination directory where the opus files will be placed. Directory structure will be preserved and any album art will be copied over.
```bash
flac2opus SRC DST
```

The `--workers` flag can be used to specify additional workers in order to encode the files in parallel.

## Contributors

Feel free to contribute by sending pull requests.

## License

Flac2opus is licensed under a BSD3-style license.
