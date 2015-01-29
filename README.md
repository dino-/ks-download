# kitchensnitch-dl


## Synopsis

Data downloader and parser for the KitchenSnitch project


## Description


## Installing


## Configuration and execution

### ks-locate - Google Places lookup utility

`ksdl.conf` and `GoogleAPIKey` files will be looked for by default
in `.` relative to where you ran `ks-locate` from. But it can be
nicer to run from within a directory tree where inspection files
and output files will be written.

To set up for running like this:

Make a directory for config files, and copy `ksdl.conf` into it,
along with your Google API key in a file. This can be anywhere as
long as they're together. How about in `$HOME`, like this:

    $HOME/
      .ksdl/
         ksdl.conf
         GoogleAPIKey

The key should be the only thing in the `GoogleAPIKey` file, on a
line by itself. 

Now, to run, have a directory structure like this:

    inspectionsToProcess/
      fail/
      insp/    <-- this is where you have downloaded insp_ json files
      succ/

Then, to run:

    $ cd inspectionsToProcess
    $ ks-locate -c $HOME/.ksdl/ -s succ/ -f fail/ --delete insp | tee ksdl.log

When it's finished:

    inspectionsToProcess/
      fail/    <-- contains failed insp_ files
      insp/    <-- this directory is now empty
      succ/    <-- contains successful lookup ks_ files

Running in this way will allow `ks-locate` to find its conf files
and use relative paths for all those directories, which is nice.


## Getting source


## Contact

### Reporting Bugs

### Authors

Dino Morelli <dino@ui3.info>


## Links
