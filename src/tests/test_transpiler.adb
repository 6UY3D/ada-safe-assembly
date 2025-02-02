-- test_transpiler.adb
with Ada.Text_IO; use Ada.Text_IO;
with Safe_Assembly_Parser;
with Safe_Assembly_Transpiler;
with Safe_Assembly_AST;
procedure Test_Transpiler is
   Test_Source : constant String :=
      "MOV R1, R2" & ASCII.LF &
      "ADD R3, R4" & ASCII.LF &
      "SUB R5, R6";
   AST     : Safe_Assembly_AST.Safe_Assembly_Block;
   LLVM_IR : String;
begin
   Put_Line("=== Test_Transpiler ===");
   AST := Safe_Assembly_Parser.Parse_Safe_Assembly(Test_Source);
   LLVM_IR := Safe_Assembly_Transpiler.Transpile_To_LLVM(AST);
   Put_Line("Generated LLVM IR:");
   Put_Line(LLVM_IR);
   New_Line;
end Test_Transpiler;
