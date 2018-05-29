The following is the plan of how to offer the current functionality (minus some bugs) in smaller phases.

Source code  
———TesslaParser—>  
Tessla  
———Flattener—> (turns nested expressions into a flat, three-address-code-like structure; also removes and replaces alpha-numeric identifiers with numeric IDs that are unique across the whole program; original names are kept around for error messages and debugging purposes; adds scope objects to the global and macro scopes, which map identifiers to their associated expressions (replacing the list of statements), and produces errors for conflicting definitions in the same scope; block expressions are removed and hoisted into the surrounding macro scope or the global scope)  
FlatTessla  
———TypeChecker—> (finds type errors, annotates every sub-expression with its type and auto-converts constant expressions to `default(nil, value)`-streams where needed)  
TypedTessla  
———ConstantEvaluator—> (evaluates constant expressions and expands macros)  
TesslaCore  
———Cycle detection—> (produces error for recursive streams without last or delayedLast)  
TesslaCore

Additional optimizations (such as common subexpression elimination and dead code elimination) will be added as a TesslaCore->TesslaCore phase after cycle detection.

To implement objects and a standard library + module system based on objects, the phases will be ammended as follows:

Source code  
———TesslaParser—>  
Tessla  
———Flattener—> (now also turns object member lists into maps and resolves import statements (by adding imported object members to the current scope's map); if an identifier isn't defined in the current scope, it checks whether a module with that name exists in the stdlib or the current project and adds that as an object to the specification)  
FlatTessla  
———TypeChecker—>  
TypedTessla  
———ConstantEvaluator—>  
MacrolessTessla  
———ObjectRemover—> (turns objects into separate variables)  
TesslaCore  
———CycleDetection—>  
TesslaCore
