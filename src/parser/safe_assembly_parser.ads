-- safe_assembly_parser.ads
with Safe_Assembly_AST;
package Safe_Assembly_Parser is

   -- Parses safe assembly source code into an AST.
   function Parse_Safe_Assembly(Source : in String) return Safe_Assembly_AST.Safe_Assembly_Block;

end Safe_Assembly_Parser;
