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


### ks-dl-nightly.sh - Utility to run every day to do everything

For `nc_wake`, no inspections are added on weekend days. The `cron`
job shoulld look something like this:

    7 1 * * tue,wed,thu,fri,sat  /opt/ks-download/bin/ks-dl-nightly.sh


## Building from source

Make sure you have `ghc 7.8.x`, `cabal-install` and `darcs` installed.

Update your cabal list

    $ cabal update

And install some native deps that `cabal` can't do for you

On Ubuntu:

    # apt-get install --reinstall g++ 
    # apt-get install libzip-dev

On Arch Linux:

    # pacman -S libzip

Get the `ks-download` source code

    $ darcs get http://hub.darcs.net/dino/ks-download

Update your cabal library and tools, we need a modern version

    $ cabal install Cabal cabal-install

Set up a sandbox for building (if you wish to use a sandbox)

    $ mkdir ~/.cabal/sandbox
    $ cabal sandbox init --sandbox=$HOME/.cabal/sandbox/kitchensnitch

Then install the dependencies

    $ cabal install --enable-tests --only-dep

This will build for quite some time, when it's done, you can build
ks-download:


### Building for development

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

And you should be good for development from here.


### Building for deployment

This will build everything into a deployable directory structure
that you can put somewhere like `/opt/` for instance.

    $ cabal configure --prefix=/tmp/ks-download-VER
    $ cabal build
    $ cabal copy
    $ cd tmp
    $ tar czvf ks-download-VER ks-download-VER


## Contact

### Reporting Bugs

### Authors

Dino Morelli <dino@ui3.info>


## Links
