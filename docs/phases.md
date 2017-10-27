The following is the plan of how to offer the current functionality (minus some bugs) in smaller phases.

Source code  
———TesslaParser—>  
Tessla  
———Scoper—> (adds maps to the global and block scopes, which map IDs to expressions, (replacing the list of statements) and produces errors for conflicting definitions in the same scope)  
ScopedTessla  
———MacroExpander—> (expands macros, multiple definitions with the same arity expand to an overoad operator (we can't properly resolve overloads by type until the type checker runs))  
MacrolessTessla  
———Uniquifier—> (removes and replaces alpha-numeric identifiers with numeric IDs that are unique across the whole program; original names are kept around for error messages and debugging purposes; block-expressions are removed and their definitions hoisted into the global map)  
UniqueTessla  
———Flattener—> (turns nested expressions into a flat, three-address-code-like structure)  
FlatTessla  
———TypeChecker—> (finds type errors, resolves overloads and auto-converts constant expressions to `default(nil, value)`-streams where needed)
TypedTessla  
———ConstantFolder—> (evaluates constant expressions, inserts overload nodes for overloaded macros)  
TesslaCore  
———DeadStreamEliminator—> (removes streams not used by outstreams)  
TesslaCore

After the current functionality is implemented, objects and a standard library + module system based on objects will be implemented next. Then the phases should look like this:

Source code  
———TesslaParser—>  
Tessla  
———Scoper—> (now also turns object member lists into maps and resolves import statements (by adding imported object members to the current scope's map); if an identifier isn't defined in the current scope, it checks whether a module with that name exists in the stdlib or the current project and adds that as an object to the specification)  
ScopedTessla  
———MacroExpander—>  
MacrolessTessla  
———Uniquifier—>  
UniqueTessla  
———Flattener—>  
FlatTessla  
———TypeChecker—>  
TypedTessla  
———ObjectRemover—> (turns objects into separate variables)  
PrimitiveTessla  
———ConstantFolder—>  
TesslaCore  
———DeadStreamEliminator—>  
TesslaCore
