Source code  
———TesslaParser—>  
Tessla  
———Uniquifier—> (resolves scopes and replaces alpha-numeric identifiers with numeric IDs that are unique across the whole program; original names are kept around for error messages and debugging purposes; block-expressions are removed and their definitions hoisted - macros now directly contain inner definitions as they still need them)  
UniqueTessla  
———Flattener—> (turns nested expressions into a flat, three-address-code-like structure)  
FlatTessla  
———Evaluator—> (evaluates macro calls and constant expressions, inserts overload nodes for overloaded macros)  
UntypedTesslaCore  
———TypeChecker—> (finds type errors, resolves overloads and auto-converts constant values to `default(nil, value)`-streams where needed)
TesslaCore  
———DeadStreamEliminator—> (removes streams not used by outstreams)  
TesslaCore
