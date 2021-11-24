# Types of the generated monitor 

For the interaction with a generated monitor API or interaction with Scala externs it may be useful to know how TeSSLa types are translated in the monitor.
This document contains an overview of this type translation:


| TeSSLa Type   | Scala Type                | Notes                                                     |
|---            |---                        |---                                                        |
|  Unit         | Boolean                   | always true                                               |
|  Bool         | Boolean                   |                                                           |
|  Int          | Long                      |                                                           |
|  Float        | Double                    |                                                           |
|  String       | String                    |                                                           |
|  Set          | scala.immutable.Set       |                                                           |
|  List         | scala.immutable.List      |                                                           |
|  Map          | scala.immutable.Map       |                                                           |
|  Tuple        | Scala Tuple               |                                                           |
|  Record       | Scala Tuple               | in the alpahnumeric order of the field names              |
