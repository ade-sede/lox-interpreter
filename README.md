["Build your own Interpreter" Challenge](https://app.codecrafters.io/courses/interpreter/overview).  
Following the book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.

# Dependencies

- [OCaml](https://ocaml.org/install#linux_mac_bsd)
- [Dune](https://github.com/ocaml/dune)
- Dune is most often installed via [opam](https://opam.ocaml.org)

# Running a script

```bash
$> cat test.lox
var hello = "Hello";
{
    var hello = "Guten Tag";
    print hello + ", Reader!";
}
print hello + ", Reader!";

$> ./your_program.sh run test.lox
Guten Tag, Reader!
Hello, Reader!
```

Alternatively to running you can:

- `tokenize` to see the list of tokens in the source file

# Status

- Lexer: OK
- Recursive descent parser: OK
- Expression evaluator: OK
- Statements & Expression statements & Scope: OK
- Control flow: TODO
