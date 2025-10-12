Compile options:
```bash
-DAST_DEBUG_ON=1
```
Will output Parser's AST contents

**Indicators of error**:
- `^0` describes a pointer to an object that was not created with `Ast::mk`. **These should never be in the output**!
- `^N` signifies `nullptr` - ensure that all pointers of the resulting AST are correct and not null where unintended
- **Red color id** (`[1]`) - means that an Entity does not belong to any other ones (only `[1]` Block should be like this, others should be green)
- `INVALID_` - some base classes that represent Expressions in `src/include/ast/Structs.h` print this out if there's no print implementation for the code they're assigned
- `*_base` - when printed, it probably means that parser's implementation created an object of a wrong class

![AST_DEBUG_ON example](docs/images/AST_DEBUG_ON_example.png)