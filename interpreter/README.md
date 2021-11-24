#  Tessla-Interpreter

TeSSLa Interpreter is the sub-module for the direct execution of TeSSLa specifications.

## Usage

The TeSSLa interpreter can be run in the following way:

```
java -jar tessla.jar interpreter [options] [<tessla-file>] [<trace-file>]
```

Following options are available:

```
  -S, --stop-on <value>    Stop when the output stream with the given name generates its first event
  -r, --reject-undeclared-inputs
                           Throw an error if an undeclared input stream occurs in the trace data
  -a, --abort-at <value>   Stop the interpreter after a given amount of events.
  --ctf                    The trace-file with the input data is in CTF format. With this option you must specify a trace-file. stdin is not supported.
  --csv                    The trace-file or the input stream is in CSV format.

```

Input events of the TeSSLa specification can either be read from a file if `<trace-file>` is given, otherwise they are consumed from `stdin`.
The input events are given in a human readable format (see below). The outputs follow the same format.

Alternatively the `<trace-file>` can also be in Common Trace Format (CTF) or CSV. Therefore use the flags `--ctf` and `--csv`.


##  Plain-text event format

Events can then be passed to the monitor via `stdin` in the following format:

```
timestamp: streamname=value
```
or in case of unit streams

```
timestamp: streamname
```

The timestamps have to arrive in an ordered way. Events for passed timestamps will be printed in the same format, the first time a greater timestamp arrives or when the input ends (Ctrl+D/EOF).
Additional infos on the input/output format can be found in [the I/O documentation](../IO.md)

## CSV event format

A csv trace file consists of a header line starting with `ts` for the current timestamp and a list of the specification's input streams.
For each timestamp where an input stream has an event the csv trace file contains a line starting with the timestamp and the value of the input streams in the order corresponding to the header line.
If an input stream does not have an event at this timestamp its cell is left empty.

Example:

```
ts,x,y
0,5
3,,3
6,100,10
```

corresponds to the following plain-text trace

```
0: x = 5
3: y = 3
6: x = 100
6: y = 10
```

## Common Trace Format (CTF)

The TeSSLa interpreter supports the [Common Trace Format (CTF)](https://diamon.org/ctf/) as input format using the [Eclipse Tracecompass](https://www.eclipse.org/tracecompass/) as parsing library.

### Usage

You can use CTF as input format with the flag `--ctf`. Then you must specify a TeSSLa specification and a CTF directory containing the metadata and the trace files. For example

```
tessla interpreter --ctf spec.tessla ./trace-1076/
```

CTF traces are automatically split up into TeSSLa event streams carrying only events of one type. For example to get a TeSSLa stream carrying all events with the name `new_thread` you can specify the following TeSSLa stream:

```
in new_thread: Events[CTF]
```

Note the usage of the `CTF` data type which encapsulates a single CTF event.

### Accessing definitions

You can access the individual definitions of a CTF event using the accessor functions `CTF.getInt` and `CTF.getString`. Both take a CTF event as first argument and a path string as second argument. Since these functions are lifted to streams they can be directly applied to CTF event streams, too.

A path string can be a nested path descriptor. So for example the path `foo.bar` looks for a definition `bar` in the definition `foo` in the event's fields. If you want to access other properties than the fields you can add a prefix to the path. For example `context:thread_hash` looks for the attribute `thread_hash` in the event's context. The following prefixes are supported:

| Prefix          | Description                                                                            | Trace Compass API        |
| --------------- | -------------------------------------------------------------------------------------- | ------------------------ |
| `context`       | Access the context of this event within a stream.                                      | `event.getContext`       |
| `eventcontext`  | Access the context of this event without the context of the stream.                    | `event.getEventContext`  |
| `eventheader`   | Access the event header.                                                               | `event.getEventHeader`   |
| `packetcontext` | Access the context of packet the event is in.                                          | `event.getPacketContext` |
| `fields`        | Access the fields of a definition. This is the default if no explicit prefix is given. | `event.getFields`        |
