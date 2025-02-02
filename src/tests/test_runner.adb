-- test_runner.adb
with Ada.Text_IO; use Ada.Text_IO;
with Test_Parser;
with Test_Transpiler;
with Test_Error_Cases;
procedure Test_Runner is
begin
   Put_Line("=== Starting All Tests for Ada Safe Assembly Extension ===");
   New_Line;
   
   Put_Line("Running Parser Tests:");
   Test_Parser;
   Put_Line("Parser tests completed." & ASCII.LF);
   
   Put_Line("Running Transpiler Tests:");
   Test_Transpiler;
   Put_Line("Transpiler tests completed." & ASCII.LF);
   
   Put_Line("Running Error Case Tests:");
   Test_Error_Cases;
   Put_Line("Error case tests completed." & ASCII.LF);
   
   Put_Line("All tests completed.");
end Test_Runner;
