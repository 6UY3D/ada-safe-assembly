-- safe_assembly_parser.adb
with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Strings.Fixed;              use Ada.Strings.Fixed;
with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Safe_Assembly_AST;
package body Safe_Assembly_Parser is

   -- Maximum number of instructions we expect (for simplicity).
   Max_Instructions : constant Positive := 100;

   -- Helper: Trim leading and trailing spaces from a string.
   function Trim(Line : in String) return String is
      First, Last : Natural;
   begin
      First := Line'First;
      while (First <= Line'Last) and then (Line(First) = ' ') loop
         First := First + 1;
      end loop;
      Last := Line'Last;
      while (Last >= First) and then (Line(Last) = ' ') loop
         Last := Last - 1;
      end loop;
      if First > Last then
         return "";
      else
         return Line(First .. Last);
      end if;
   end Trim;

   -- Helper: Splits the source text into lines (using LF as a delimiter).
   function Split_Lines(Source : in String) return Safe_Assembly_AST.Instruction_Array is
      -- We'll use a fixed-size temporary array.
      type Temp_Array is array (1 .. Max_Instructions) of String(1 .. Source'Length);
      Temp : Temp_Array;
      Count : Natural := 0;
      Start_Index : Positive := Source'First;
   begin
      for I in Source'Range loop
         if Source(I) = ASCII.LF then
            if I > Start_Index then
               Count := Count + 1;
               Temp(Count) := Source(Start_Index .. I - 1);
            end if;
            Start_Index := I + 1;
         end if;
      end loop;
      if Start_Index <= Source'Last then
         Count := Count + 1;
         Temp(Count) := Source(Start_Index .. Source'Last);
      end if;
      declare
         Result : Safe_Assembly_AST.Instruction_Array(1 .. Count);
      begin
         for J in 1 .. Count loop
            -- Initially set each instruction with the entire trimmed line as mnemonic.
            Result(J) := (Mnemonic => Trim(Temp(J)) & (if Trim(Temp(J))'Length < 16 then (others => ' ') else (others => ' ')),
                          Operands => (others => ' '));
         end loop;
         return Result;
      end;
   end Split_Lines;

   -- Helper: Parse a single line into an Instruction record.
   function Parse_Line(Line : in String) return Safe_Assembly_AST.Instruction is
      Clean_Line : constant String := Trim(Line);
      Space_Pos : Natural := 0;
      Mnemonic, Operands : String(1 .. Clean_Line'Length) := (others => ' ');
   begin
      -- Find the first space to separate mnemonic and operands.
      for I in Clean_Line'Range loop
         if Clean_Line(I) = ' ' then
            Space_Pos := I;
            exit;
         end if;
      end loop;
      if Space_Pos = 0 then
         Mnemonic := Clean_Line;
         Operands := "";
      else
         Mnemonic := Trim(Clean_Line(Clean_Line'First .. Space_Pos - 1));
         if Space_Pos < Clean_Line'Last then
            Operands := Trim(Clean_Line(Space_Pos + 1 .. Clean_Line'Last));
         else
            Operands := "";
         end if;
      end if;
      -- Right-pad the fields to the fixed sizes.
      declare
         Mnemonic_Field : String(1 .. 16) := (others => ' ');
         Operands_Field : String(1 .. 32) := (others => ' ');
      begin
         if Mnemonic'Length <= 16 then
            Mnemonic_Field(1 .. Mnemonic'Length) := Mnemonic;
         else
            Mnemonic_Field := Mnemonic(1 .. 16);
         end if;
         if Operands'Length <= 32 then
            Operands_Field(1 .. Operands'Length) := Operands;
         else
            Operands_Field := Operands(1 .. 32);
         end if;
         return (Mnemonic => Mnemonic_Field, Operands => Operands_Field);
      end;
   end Parse_Line;

   -- Helper: Validate that the mnemonic is in the allowed list.
   function Is_Allowed_Mnemonic(Mnemonic : in String) return Boolean is
      Allowed : constant array(1 .. 5) of String := ("MOV", "ADD", "SUB", "MUL", "DIV");
   begin
      for I in Allowed'Range loop
         if Allowed(I) = Trim(Mnemonic) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Allowed_Mnemonic;

   -- Main parsing function.
   function Parse_Safe_Assembly(Source : in String) return Safe_Assembly_AST.Safe_Assembly_Block is
      Raw_Lines : constant Safe_Assembly_AST.Instruction_Array := Split_Lines(Source);
      Temp_Instructions : array(1 .. Max_Instructions) of Safe_Assembly_AST.Instruction;
      Count : Natural := 0;
   begin
      for I in Raw_Lines'Range loop
         declare
            Parsed : Safe_Assembly_AST.Instruction := Parse_Line(Raw_Lines(I).Mnemonic);
         begin
            if Parsed.Mnemonic /= "" then
               if Is_Allowed_Mnemonic(Parsed.Mnemonic) then
                  Count := Count + 1;
                  Temp_Instructions(Count) := Parsed;
               else
                  Put_Line("Warning: Disallowed mnemonic encountered: " & Trim(Parsed.Mnemonic));
               end if;
            end if;
         end;
      end loop;
      if Count = 0 then
         -- Return an empty block if no valid instructions.
         return (Instructions => (others => (Mnemonic => (others => ' '), Operands => (others => ' '))));
      else
         declare
            Result : Safe_Assembly_AST.Instruction_Array(1 .. Count);
         begin
            for J in 1 .. Count loop
               Result(J) := Temp_Instructions(J);
            end loop;
            return (Instructions => Result);
         end;
      end if;
   end Parse_Safe_Assembly;

end Safe_Assembly_Parser;
