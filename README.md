## Project init
1. **Download LLVM pre-build binaries** at https://github.com/llvm/llvm-project/releases/tag/llvmorg-18.1.8
2. Uncompress the file, name it `llvm` and put it in `external/`
```bash
cd ./external/
wget https://github.com/llvm/llvm-project/releases/download/llvmorg-18.1.8/clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz
tar -xf clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz
mv clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04 llvm
rm -rf clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz
cd ..
```

## Compile options:
```bash
-DAST_DEBUG_ON=1
```
Will output Parser's AST contents

**Indicators of error**:
- `^0` describes a pointer to an object that was not created with `Ast::mk`. **These should never be in the output**!
- `^N` signifies `nullptr` - ensure that all pointers of the resulting AST are correct and not null where unintended
- `INVALID_` - some base classes that represent Expressions in `src/include/ast/Structs.h` print this out if there's no print implementation for the code they're assigned
- `*_base` - when printed, it probably means that parser's implementation created an object of a wrong class
- **Orphan nodes** before Analyzer stage
- **Bold red IdRef, RoutineCall, TypeRef** - any structs that have a **.ref** field, they are printed as bright red when the ref field is null (**should never happen if Analyzer has already run**)

![AST_DEBUG_ON example](docs/images/AST_DEBUG_ON_example.png)

## Notes
- Assignment in global scope is only legal if it can be elided at compile time (permitted because there is no array/record init syntax)