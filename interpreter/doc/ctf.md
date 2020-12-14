# Common Trace Format (CTF)

The TeSSLa interpreter supports the [Common Trace Format (CTF)](https://diamon.org/ctf/) as input format using the [Eclipse Tracecompass](https://www.eclipse.org/tracecompass/) as parsing library.

## Usage

You can use CTF as input format with the flag `--ctf`. Then you must specify a TeSSLa specification and a CTF directory containing the metadata and the trace files. For example

```
tessla interpreter --ctf spec.tessla ./trace-1076/
```

CTF traces are automatically split up into TeSSLa event streams carrying only events of one type. For example to get a TeSSLa stream carrying all events with the name `new_thread` you can specify the following TeSSLa stream:

```
in new_thread: Events[CTF]
```

Note the usage of the `CTF` data type which encapsulates a single CTF event.

## Accessing definitions

You can access the individual definitions of a CTF event using the accessor functions `CTF.getInt` and `CTF.getString`. Both take a CTF event as first argument and a path string as second argument. Since these functions are lifted to streams they can be directly applied to CTF event streams, too.

A path string can be a nested path descriptor. So for example the path `foo.bar` looks for a definition `bar` in the definition `foo` in the event's fields. If you want to access other properties than the fields you can add a prefix to the path. For example `context:thread_hash` looks for the attribute `thread_hash` in the event's context. The following prefixes are supported:

| Prefix          | Description                                                                            | Trace Compass API        |
| --------------- | -------------------------------------------------------------------------------------- | ------------------------ |
| `context`       | Access the context of this event within a stream.                                      | `event.getContext`       |
| `eventcontext`  | Access the context of this event without the context of the stream.                    | `event.getEventContext`  |
| `eventheader`   | Access the event header.                                                               | `event.getEventHeader`   |
| `packetcontext` | Access the context of packet the event is in.                                          | `event.getPacketContext` |
| `fields`        | Access the fields of a definition. This is the default if no explicit prefix is given. | `event.getFields`        |
