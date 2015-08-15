# gen-phzr

Generate phzr clojurescript files based on JSON data built from parsing jsdoc
comments in Phaser javascript source.


## Usage

### Prerequisites

* JVM
* [leiningen](http://leiningen.org/)
* [Node.js](https://nodejs.org/)


### Generate jsdoc JSON

To generate the `resources/phaser.json` file required by gen-phzr, first
change into the `resources/phaser-src` directory. Then run the following
commands:

```bash
grunt clean
grunt pixidoc
grunt gitclone:plugins
grunt jsdoc:json
```

Install whatever grunt/node packages are necessary to make those commands run
successfully. Once they've all run without error, copy the generated JSON file
into the gen-phzr project resources:

```bash
cp out/docs.json ../phaser.json
```


### Generate phzr code

Change back into the gen-phzr project root directory and run:

```bash
lein run
```

All generated files will be copied into the `out/` directory.


## Manual Edits

Some generated files require hand-tweaking because:

1. I am *lazy* and the code to fix them programmatically is somewhat cumbersome

Apply the following changes once the source files have been generated:

* 2015-08-15 - The `array-set/call-all` function should take varargs. This is
not conveyed by the jsdoc JSON clearly. The function def should look like:
```clojure
(defn call-all
"Calls a function on all members of this list, using the member as the context
for the callback.

  If the `key` property is present it must be a function.
  The function is invoked using the item as the context.

  Parameters:
    * array-set (Phaser.ArraySet) - Targeted instance for method
    * key (string) - The name of the property with the function to call.
    * parameters (*) - Additional parameters that will be passed to the callback."
  ([array-set key & parameters]
   (phaser->clj
    (.callAll array-set
              (clj->phaser key)
              (clj->phaser parameters)))))
```

### Bugs

I'm sure there are some. This app was cobbled together hastily in order to get
phzr up and running. Maybe someone will find it useful? I hope so!


Copyright © 2015 Dylan Paris

Distributed under the MIT license.
