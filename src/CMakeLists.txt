cmake_minimum_required(VERSION 3.8)

project(silcc VERSION 0.0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

### Packages
# set(llvm_path ${CMAKE_SOURCE_DIR}/../external/llvm)
# include_directories(${llvm_path}/include)
# link_directories(${llvm_path}/lib)
find_package(LLVM REQUIRED)

### Project
add_executable(${PROJECT_NAME} main.cpp)

set(LIB_NAME ${PROJECT_NAME}_lib)
add_library(${LIB_NAME}
    ${CMAKE_SOURCE_DIR}/lexer/Lexer.cpp
    ${CMAKE_SOURCE_DIR}/lexer/Token.cpp
    ${CMAKE_SOURCE_DIR}/parser/Ast.cpp
    ${CMAKE_SOURCE_DIR}/parser/Parser.cpp
    ${CMAKE_SOURCE_DIR}/parser/srlz.cpp
    ${CMAKE_SOURCE_DIR}/parser/DebugTree.cpp
    ${CMAKE_SOURCE_DIR}/analyzer/Analyzer.cpp
    ${CMAKE_SOURCE_DIR}/analyzer/Optimizer.cpp
    ${CMAKE_SOURCE_DIR}/analyzer/utils.cpp
    ${CMAKE_SOURCE_DIR}/codegen/Codegen.cpp
)

# target_link_libraries(${LIB_NAME}
#     LLVMMC
#     LLVMDWP
#     LLVMLTO
#     LLVMMCA
#     LLVMipo
#     LLVMCore
#     LLVMDiff
#     LLVMXRay
#     LLVMMCJIT
#     LLVMLinker
#     LLVMObject
#     LLVMOption
#     LLVMOrcJIT
#     LLVMPasses
#     LLVMTarget
#     LLVMVEDesc
#     LLVMVEInfo
#     LLVMARMDesc
#     LLVMARMInfo
#     LLVMAVRDesc
#     LLVMAVRInfo
#     LLVMBPFDesc
#     LLVMBPFInfo
#     LLVMCFGuard
#     LLVMCodeGen
#     LLVMJITLink
#     LLVMObjCopy
#     LLVMRemarks
#     LLVMSupport
#     LLVMTextAPI
#     LLVMX86Desc
#     LLVMX86Info
#     LLVMARMUtils
#     LLVMAnalysis
#     LLVMCoverage
#     LLVMDemangle
#     LLVMIRReader
#     LLVMMCParser
#     LLVMMipsDesc
#     LLVMMipsInfo
#     LLVMTableGen
#     LLVMAsmParser
#     LLVMBitReader
#     LLVMBitWriter
#     LLVMFileCheck
#     LLVMFuzzerCLI
#     LLVMHipStdPar
#     LLVMIRPrinter
#     LLVMLanaiDesc
#     LLVMLanaiInfo
#     LLVMLibDriver
#     LLVMMIRParser
#     LLVMNVPTXDesc
#     LLVMNVPTXInfo
#     LLVMOrcShared
#     LLVMRISCVDesc
#     LLVMRISCVInfo
#     LLVMSparcDesc
#     LLVMSparcInfo
#     LLVMSymbolize
#     LLVMVECodeGen
#     LLVMVectorize
#     LLVMXCoreDesc
#     LLVMXCoreInfo
#     LLVMAMDGPUDesc
#     LLVMAMDGPUInfo
#     LLVMARMCodeGen
#     LLVMAVRCodeGen
#     LLVMAsmPrinter
#     LLVMBPFCodeGen
#     LLVMCoroutines
#     LLVMDebuginfod
#     LLVMExtensions
#     LLVMFuzzMutate
#     LLVMGlobalISel
#     LLVMLineEditor
#     LLVMMSP430Desc
#     LLVMMSP430Info
#     LLVMObjectYAML
#     LLVMScalarOpts
#     LLVMX86CodeGen
#     LLVMAArch64Desc
#     LLVMAArch64Info
#     LLVMAMDGPUUtils
#     LLVMDWARFLinker
#     LLVMHexagonDesc
#     LLVMHexagonInfo
#     LLVMInstCombine
#     LLVMInterpreter
#     LLVMMipsCodeGen
#     LLVMObjCARCOpts
#     LLVMPowerPCDesc
#     LLVMPowerPCInfo
#     LLVMProfileData
#     LLVMRuntimeDyld
#     LLVMSystemZDesc
#     LLVMSystemZInfo
#     LLVMVEAsmParser
#     LLVMAArch64Utils
#     LLVMARMAsmParser
#     LLVMAVRAsmParser
#     LLVMBPFAsmParser
#     LLVMBinaryFormat
#     LLVMCodeGenTypes
#     LLVMDebugInfoBTF
#     LLVMDebugInfoMSF
#     LLVMDebugInfoPDB
#     LLVMFrontendHLSL
#     LLVMLanaiCodeGen
#     LLVMNVPTXCodeGen
#     LLVMOrcDebugging
#     LLVMRISCVCodeGen
#     LLVMSelectionDAG
#     LLVMSparcCodeGen
#     LLVMTargetParser
#     LLVMX86AsmParser
#     LLVMX86TargetMCA
#     LLVMXCoreCodeGen
#     LLVMAMDGPUCodeGen
#     LLVMDebugInfoGSYM
#     LLVMDlltoolDriver
#     LLVMInterfaceStub
#     LLVMLoongArchDesc
#     LLVMLoongArchInfo
#     LLVMMSP430CodeGen
#     LLVMMipsAsmParser
#     LLVMWindowsDriver
#     LLVMAArch64CodeGen
#     LLVMDebugInfoDWARF
#     LLVMFrontendDriver
#     LLVMFrontendOpenMP
#     LLVMHexagonCodeGen
#     LLVMLanaiAsmParser
#     LLVMMCDisassembler
#     LLVMPowerPCCodeGen
#     LLVMRISCVAsmParser
#     LLVMRISCVTargetMCA
#     LLVMSparcAsmParser
#     LLVMSystemZCodeGen
#     LLVMTableGenCommon
#     LLVMTransformUtils
#     LLVMVEDisassembler
#     LLVMAMDGPUAsmParser
#     LLVMAMDGPUTargetMCA
#     LLVMARMDisassembler
#     LLVMAVRDisassembler
#     LLVMBPFDisassembler
#     LLVMBitstreamReader
#     LLVMExecutionEngine
#     LLVMFrontendOpenACC
#     LLVMInstrumentation
#     LLVMMSP430AsmParser
#     LLVMWebAssemblyDesc
#     LLVMWebAssemblyInfo
#     LLVMWindowsManifest
#     LLVMX86Disassembler
#     LLVMAArch64AsmParser
#     LLVMHexagonAsmParser
#     LLVMLoongArchCodeGen
#     LLVMMipsDisassembler
#     LLVMOrcTargetProcess
#     LLVMPowerPCAsmParser
#     LLVMSystemZAsmParser
#     LLVMWebAssemblyUtils
#     LLVMDebugInfoCodeView
#     LLVMLanaiDisassembler
#     LLVMRISCVDisassembler
#     LLVMSparcDisassembler
#     LLVMXCoreDisassembler
#     LLVMAMDGPUDisassembler
#     LLVMDWARFLinkerClassic
#     LLVMFrontendOffloading
#     LLVMLoongArchAsmParser
#     LLVMMSP430Disassembler
#     LLVMWebAssemblyCodeGen
#     LLVMAArch64Disassembler
#     LLVMDWARFLinkerParallel
#     LLVMHexagonDisassembler
#     LLVMPowerPCDisassembler
#     LLVMSystemZDisassembler
#     LLVMTextAPIBinaryReader
#     LLVMDebugInfoLogicalView
#     LLVMWebAssemblyAsmParser
#     LLVMAggressiveInstCombine
#     LLVMLoongArchDisassembler
#     LLVMWebAssemblyDisassembler
#     ncurses
# )
target_link_libraries(${LIB_NAME} llvm-core::llvm-core)

target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(${PROJECT_NAME} PUBLIC ${LIB_NAME})