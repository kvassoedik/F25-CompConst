#include "codegen/Codegen.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

void codegen::Codegen::build() {
    std::error_code ec;

    std::string dumpFilename = config_.buildIntoExe ? config_.outputFileName + ".ll" : config_.outputFileName;
    llvm::raw_fd_ostream dest{dumpFilename, ec, llvm::sys::fs::OF_None};

    if (ec) {
        llvm::errs() << "build error: could not open file: " << ec.message() << "\n";
        return;
    }

    module_->print(dest, nullptr);

    if (!config_.buildIntoExe) {
        return;
    }

    std::string objFilename = config_.outputFileName + ".o";
    if (system(("llc -filetype=obj -relocation-model=pic " + dumpFilename + " -o " + objFilename).c_str()) != 0)
        return;
    if (system(("gcc " + objFilename + " -o " + config_.outputFileName).c_str()) != 0)
        return;
}