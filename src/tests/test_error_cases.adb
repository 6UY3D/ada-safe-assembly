-- test_error_cases.adb
with Ada.Text_IO; use Ada.Text_IO;
with Safe_Assembly_Parser;
with Safe_Assembly_AST;
procedure Test_Error_Cases is
   Test_Source : constant String := "FOO R7, R8"; -- 'FOO' is not an allowed mnemonic.
   AST : Safe_Assembly_AST.Safe_Assembly_Block;
begin
   Put_Line("=== Test_Error_Cases ===");
   AST := Safe_Assembly_Parser.Parse_Safe_Assembly(Test_Source);
   Put_Line("Testing error case for disallowed instruction:");
   Safe_Assembly_AST.Display_AST(AST);
   New_Line;
end Test_Error_Cases;
