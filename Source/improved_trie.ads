with Ada.Containers.Multiway_Trees;
with Ada.Text_IO;
package Improved_Trie is

	type Element_Type is record
		A: Character;
		B: Integer;
	end record;
	
	function "=" (Left, Right : Element_Type) return Boolean is
		(Left.A = Right.A);

	--the Ada Multiway tree package backend to be used
	package Trie is new Ada.Containers.Multiway_Trees (Element_Type=>Element_Type, "="=>"=");
	use Trie;	

	--searches the Trie for the input string. 
	--returns the associated integer, returns -1 if not found
	function Find_String (T: Tree; Input: String) return Integer;

	--adds the input string into the Trie
	--returns whether it is successful or not
	function Add_String (T: in out Tree; Input: String; Address: Integer) return Boolean;

private

	--finds the child of a node 
	--returns the found child node, No_Element if not found
	function Find_Immediate_Child (Parent: Cursor; Element: Element_Type) return Cursor;

	--moves the input node to the found child node 
	--returns if found or not.
	--if not found, Parent is not changed 
	function Find_Move_Immediate_Child (Parent: in out Cursor; Element: Element_Type) return Boolean;
	
end Improved_Trie;