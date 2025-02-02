-- safe_assembly_transpiler.adb
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;   use Ada.Strings.Unbounded.Text_IO;
with Safe_Assembly_AST;
package body Safe_Assembly_Transpiler is

   function Transpile_To_LLVM(AST : in Safe_Assembly_AST.Safe_Assembly_Block) return String is
      IR_Code   : Unbounded_String := To_Unbounded_String("");
      New_Line  : constant Unbounded_String := To_Unbounded_String(ASCII.LF);
   begin
      for I in AST.Instructions'Range loop
         case Safe_Assembly_AST.Instruction(AST.Instructions(I)).Mnemonic is
            when "MOV" =>
               IR_Code := IR_Code & To_Unbounded_String("; MOV instruction: move value from source to destination") & New_Line &
                         To_Unbounded_String("%dest = add i32 %src, 0") & New_Line;
            when "ADD" =>
               IR_Code := IR_Code & To_Unbounded_String("; ADD instruction: add two registers") & New_Line &
                         To_Unbounded_String("%result = add i32 %reg1, %reg2") & New_Line;
            when "SUB" =>
               IR_Code := IR_Code & To_Unbounded_String("; SUB instruction: subtract two registers") & New_Line &
                         To_Unbounded_String("%result = sub i32 %reg1, %reg2") & New_Line;
            when "MUL" =>
               IR_Code := IR_Code & To_Unbounded_String("; MUL instruction: multiply two registers") & New_Line &
                         To_Unbounded_String("%result = mul i32 %reg1, %reg2") & New_Line;
            when "DIV" =>
               IR_Code := IR_Code & To_Unbounded_String("; DIV instruction: divide two registers") & New_Line &
                         To_Unbounded_String("%result = sdiv i32 %reg1, %reg2") & New_Line;
            when others =>
               IR_Code := IR_Code & To_Unbounded_String("; Unknown instruction: ") &
                          To_Unbounded_String(Safe_Assembly_AST.Instruction(AST.Instructions(I)).Mnemonic) & New_Line;
         end case;
      end loop;
      return To_String(IR_Code);
   end Transpile_To_LLVM;

end Safe_Assembly_Transpiler;
