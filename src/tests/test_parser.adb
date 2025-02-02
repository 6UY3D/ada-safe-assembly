-- test_parser.adb
with Ada.Text_IO; use Ada.Text_IO;
with Safe_Assembly_Parser;
with Safe_Assembly_AST;
procedure Test_Parser is
   Test_Source : constant String :=
      "MOV R1, R2" & ASCII.LF &
      "ADD R3, R4" & ASCII.LF &
      "INVALID R5, R6";  -- 'INVALID' is not allowed.
   AST : Safe_Assembly_AST.Safe_Assembly_Block;
begin
   Put_Line("=== Test_Parser ===");
   AST := Safe_Assembly_Parser.Parse_Safe_Assembly(Test_Source);
   Put_Line("Parsed AST:");
   Safe_Assembly_AST.Display_AST(AST);
   New_Line;
end Test_Parser;
