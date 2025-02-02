-- safe_assembly_ast.ads
package Safe_Assembly_AST is

   -- Represents a single assembly instruction.
   type Instruction is record
      -- Fixed-length fields for simplicity. In a production system, you may use dynamic strings.
      Mnemonic : String (1 .. 16);
      Operands : String (1 .. 32);
   end record;

   subtype Instruction_Index is Natural;
   type Instruction_Array is array (Instruction_Index range <>) of Instruction;

   -- Represents a safe assembly block as a collection of instructions.
   type Safe_Assembly_Block is record
      Instructions : Instruction_Array;
   end record;

   -- Debugging: Displays the AST.
   procedure Display_AST(AST : in Safe_Assembly_Block);

end Safe_Assembly_AST;
