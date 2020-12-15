# Docs

This module exports documentation information in a JSON format, which can then be processed by additional tools into various presentable and human-readable format.

## Usage

The documentation tool is integrated in TeSSLa an available as `tessla doc`. The tool generates documentation information for the given TeSSLa specification. The most important options are `--include-stdlib` to include the documentation of the standard library and `--outfile` to specify the output file.

For example the following command generates the documentation information for the standard library:

```
tessla doc --include-stdlib --outfile doc.json
```

An exhaustive list of all available options can be obtained with `tessla --help`.

## TeSSLa Doc Comments

The TeSSLa documentation tool considers all documentation comments directly before a definition, a module or a type declaration. Documentation comments start with `##` and should be in markdown. The interpretation of the markdown documentation is mainly up to the tool processing the documentation information. The TeSSLa documentation tool simply provides the markdown text as a string for further processing.

The documentation information contains all global definitions, regardless if they have a documentation comment.

Two special documentation comment lines are interpreted differently:
* `nodoc`: If a documentation comment only consists of `nodoc` then the following definition will not be included in the documentation.
* `inmodule Module`: If a documentation contains such a line then the following definition will be semantically associated to this module although it might not technically be inside the scope of the module. See `inmodule` below.

## JSON format

The JSON format is implemented in [DocJsonProtocol](src/main/scala/de/uni_luebeck/isp/tessla/tessladoc/DocJsonProtocol.scala).

On the outer level the documentation information consists of the `imports` and the `items` available on the root level. Imports simply consist of the module `path` of the imported modules and items are the different documented items. They are discussed in detail below.

### Items

Every item has the following attributes:
* `name` containing the name of the item without its signature.
* `doc` containing the documentation in markdown format.
* `loc` containing the source location of the item. A location consists of a file `path` and a `range` indicating the location of the item in that file using `fromColumn`, `fromLine`, `toColumn` and `toLine`.
* `kind` indicating the type of the item: `AnnotationDoc`, `TypeDoc`, `ModuleDoc` or `DefDoc`.

#### AnnotationDoc

Documents an annotation. Has the following additional attributes:
* `global` is a boolean value indicating if the annotation is a global annotation.
* `inModule` is an optional string with the value of the `inmodule` special comment (see above).
* `parameters` is a list of parameters. See below.

#### TypeDoc

Documents a type definition. Has the following additional attributes:
* `inModule` is an optional string with the value of the `inmodule` special comment (see above).
* `typeParameters` is a list of strings with the names of the type parameters.

#### ModuleDoc

Documents a module definition. Has the additional attributes `members` and `imports` working the same way as their counterparts on the root level.

#### DefDoc

Documents a definition. Has the following additional attributes:
* `src` is a string containing the entire source code of the definition including its name and signature.
* `typeParameters` is a list of strings with the names of the type parameters.
* `parameters` is a list of parameters. See below.
* `returnType` is an optional attribute containing the return type as a type object (see below).
* `isLiftable` is a boolean attribute indicating if the definition was declared liftable.

#### Parameter and Types

A parameter consists of a string `name` and an _type object with evaluation_ `typ`. The type object with evaluation has an evaluation strategy `eval` and the _type object_ `typ`. The evaluation strategy `eval` is a string containing either `strict`, `lazy` or nothing. The type object `typ` can be one of the following indicated by the `kind` attribute:
* `SimpleType`: A simple type has just a `name`.
* `TypeApplication`: A type application has a `constructor` which is another type and `arguments` which is a list of types.
* `FunctionType`: A function type has `parameters` which is a list of type objects with evaluation and a `result` which is a type.
* `ObjectType`: An object type has `members` which is an object with names as keys and types as values.
* `TupleType`: A tuple type has `members` which is a list of types.

### Example

The following example is an extract of the documentation of the TeSSLa standard library for the `getSome` macro:

```json
{
  "name":"getSome",
  "doc":"Get the value contained in a @[#Some]. If the given option is a @[#None], a run-time error will occur",
  "isLiftable":true,
  "kind":"DefDoc",
  "loc":{
    "path":"Option.tessla",
    "range":{
      "fromColumn":3,
      "fromLine":128,
      "toColumn":72,
      "toLine":129
    }
  },
  "parameters":[
    {
      "name":"opt",
      "typ":{
        "eval":"strict",
        "typ":{
          "arguments":[
            {
              "kind":"SimpleType",
              "name":"T"
            }
          ],
          "constructor":{
            "kind":"SimpleType",
            "name":"Option"
          },
          "kind":"TypeApplication"
        }
      }
    }
  ],
  "returnType":{
    "kind":"SimpleType",
    "name":"T"
  },
  "src":"liftable def getSome[T](opt: strict Option[T]): T = extern(\"getSome\")",
  "typeParameters":[
    "T"
  ]
}
```