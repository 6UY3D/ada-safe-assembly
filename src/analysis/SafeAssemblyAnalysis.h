// SafeAssemblyAnalysis.h
#ifndef SAFE_ASSEMBLY_ANALYSIS_H
#define SAFE_ASSEMBLY_ANALYSIS_H

#include "llvm/IR/PassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {

class SafeAssemblyAnalysisPass : public PassInfoMixin<SafeAssemblyAnalysisPass> {
public:
    // Runs the pass on a given function and emits warnings if unsafe patterns are found.
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

} // end namespace llvm

#endif // SAFE_ASSEMBLY_ANALYSIS_H
