-- safe_assembly_ast.adb
with Ada.Text_IO; use Ada.Text_IO;

package body Safe_Assembly_AST is

   procedure Display_AST(AST : in Safe_Assembly_Block) is
   begin
      for I of AST.Instructions loop
         if I.Mnemonic /= (others => ' ') then
            Put_Line("Mnemonic: " & I.Mnemonic);
            Put_Line("Operands: " & I.Operands);
            New_Line;
         end if;
      end loop;
   end Display_AST;

end Safe_Assembly_AST;
