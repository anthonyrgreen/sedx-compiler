# sedx-compiler

Regexes should be easy to write, easy to read, and easy to run. Regrettably, only the third is true of `sed`-style regexes. The aim of this project is to create a mini-language for building regexes. Its programs can be transpiled into sed-style regexes and run easily.

An example program:

    let
      single_import := '${star(anyof('A-Za-z0-9')), maybe(', ')}'
      many_imports := '${star(single_import())}'
      filename := '${star(noneof('/ '))}'
      whole_path := '${capture(filename()) as root, '/', star(filename(), '/'), capture(filename()) as filename}'
    match
    import {${capture(many_imports()) as imports}} from '${capture(whole_path()) as whole_path}';
    substitute
    import {ExtraImport, ${imports}} from '${whole_path.root}/some/directory/string/${whole_path.filename}';

This program produces a sed-style regex which transforms the string 

    import {Module1, Module2} from 'root/of/my/initial/file';

into the string

    import {ExtraImport, Module1, Module2} from 'root/move/to/some/new/directory/file';

In particular, it will produce:

    s/import {\(\([A-Za-z0-9]*\(, \)\?\)*\)} from '\(\([^/ ]*\)\/\([^/ ]*\/\)*\([^/ ]*\)\)'/import {ExtraImport, \1} from'\5\/move\/to\/some\/new\/directory\/\7/g

Copying this to sed...

    $ echo "import {Module1, Module2} from 'root/of/my/initial/file'" \
        | sed -E "s/import {\(\([A-Za-z0-9]*\(, \)\?\)*\)} from '\(\([^/ ]*\)\/\([^/ ]*\/\)*\([^/ ]*\)\)'/import {ExtraImport, \1} from'\5\/move\/to\/some\/new\/directory\/\7/g"
    import {Module1, Module2} from 'root/move/to/some/new/directory/file

Some other expressions you will be able to use (soon!)
    
    a_lot_of_characters := '${star(any())}'
    at_least_one_hyphen := '${plus('-')}'
    capturing_multiple_components_at_once := '${capture('hello', ' world') as hello_world_alias}'


## TODOs

- [ ] Brackets do not handle collating symbols.
- [x] `?` and `*` always cause capture groups to be created. They needn't if applied to a single character.
- [x] We should add the option to remove unused capture groups.
- [ ] Functions, like capture groups, should accept multiple sub-expressions as arguments
- [ ] Compile-time checking of regex validity
- [x] Moves [ to the front of a character class when it is not a part of a collating symbol
- [ ] User-defined functions can accept arguments?
- [x] Add `plus()`, `any()`, `start()`, `atLeast()`, `atMost()`, and `end()`
- [ ] Add `exactly()`
