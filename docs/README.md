# Docs

This module allows generation of documentation information in a JSON format, which can then be used by other backends to process into a presentable and human-readable format.

# JSON format

Details on the JSON format can be found in the used [JsonProtocol](src/main/scala/de/uni_luebeck/isp/tessla/tessladoc/DocJsonProtocol.scala). 

The following example is an extract of the documentation of the TeSSLa standard library, for the `getSome` macro:

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