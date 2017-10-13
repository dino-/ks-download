# ks-download


## Synopsis

Data downloader and parser for the KitchenSnitch project (Haskell)


## Description


## Installing


## Configuration and execution

Make a directory for config files, and copy or link the conf files into
it, along with your Google API key in a file. The defaults are
expecting `$HOME/.config/kitchensnitch`

      $HOME/
         .config/
            kitchensnitch/
               GoogleAPIKey
               ks-locate.conf
               mongodb.conf

The key should be the only thing in the `GoogleAPIKey` file, on a
line by itself. 


### ks-locate - Google Places lookup utility

To run, have a directory structure like this:

    inspectionsToProcess/
      fail/
      insp/    <-- this is where you have downloaded insp_ json files
      succ/

Then, to run:

    $ cd inspectionsToProcess
    $ ks-locate -c $HOME/.config/kitchensnitch -s succ/ -f fail/ --delete insp | tee ks-locate.log

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

    7 1 * * wed,thu,fri,sat,sun  /opt/ks-download/bin/ks-dl-nightly.sh


### ks-regionupd - Update of regional statistics, to be run daily

Put this in a cron job:

    45 1 * * *  /opt/ks-download/bin/ks-regionupd --log-priority=NOTICE $HOME/.config/kitchensnitch > /some/dir/ks-regionupd.log


## Building from source

Follow the instructions on the wiki for setting up a sandbox for all KS development, in the section "Building the KitchenSnitch Haskell server-side components for deployment"


### Building for development

      $ cabal sandbox init --sandbox=/home/USER/.cabal/sandbox/kitchensnitch
      $ cabal install --only-dep --enable-tests
      $ cabal configure --enable-tests
      $ cabal build
      $ cabal test

And you should be good for development from here.


### Building for deployment

This will build everything into a deployable directory structure
that you can put somewhere like `/opt/` for instance.

    $ cabal install --prefix=/tmp/ks-download-VER --datasubdir=.
    $ pushd /tmp
    $ tar czvf ks-download-VER.tgz ks-download-VER
    $ popd

Some of these notes exist in a more detailed form on the developer wiki.

When you want to insert records into the database, make sure you do not forget to edit the `bin/ks-dl-nightly.sh` script. By default ks-dbinsert execution is commented out and fake one runs!


## Contact

### Reporting Bugs

### Authors

Dino Morelli <dino@ui3.info>


## Links
