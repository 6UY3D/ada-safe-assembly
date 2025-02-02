-- safe_assembly_transpiler.ads
with Safe_Assembly_AST;
package Safe_Assembly_Transpiler is

   -- Transpiles a safe assembly AST into LLVM IR code.
   function Transpile_To_LLVM(AST : in Safe_Assembly_AST.Safe_Assembly_Block) return String;

end Safe_Assembly_Transpiler;
