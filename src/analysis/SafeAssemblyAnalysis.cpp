// SafeAssemblyAnalysis.cpp
#include "SafeAssemblyAnalysis.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

PreservedAnalyses SafeAssemblyAnalysisPass::run(Function &F, FunctionAnalysisManager &AM) {
    errs() << "Running SafeAssemblyAnalysisPass on function: " << F.getName() << "\n";
    
    for (auto &BB : F) {
        for (auto &I : BB) {
            // Check for 'add' instructions that might simulate MOV.
            if (I.getOpcodeName() == StringRef("add")) {
                if (auto *ConstOp = dyn_cast<ConstantInt>(I.getOperand(1))) {
                    if (ConstOp->isZero()) {
                        errs() << "Note: 'add' instruction with 0 constant detected (possible MOV simulation): " << I << "\n";
                    }
                }
            }
            // Warn if any call instruction is found (which might be unsafe in this context).
            if (isa<CallInst>(I)) {
                errs() << "Warning: Call instruction found. Verify safety constraints: " << I << "\n";
            }
        }
    }
    
    return PreservedAnalyses::all();
}
