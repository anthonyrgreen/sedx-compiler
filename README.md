# sed-transpile

Regexes should be easy to write, easy to read, and easy to run. Regrettably, only the third is true of `sed`-style regexes. The aim of this project is to create a mini-language for building regexes. Its programs can be transpiled into sed-style regexes and run easily.

An example program:

    let
      single_import := '${star(anyof('A-Za-z')), maybe(,), ' '}'
      many_imports := '${star(single_import())}'
      filename := '${star(noneof('/ '))}'
      whole_path := '${capture(filename()) as root, '/', star(filename(), '/'), capture(filename()) as filename}'
    in
    match
    import {${capture(many_imports()) as imports}} from '${capture(whole_path()) as api_codepath}';
    substitute
    import {${imports}} from '${whole_path.root}/some/directory/string/${whole_path.filename}'



