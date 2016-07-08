with Ada.Containers.Multiway_Trees;

package Improved_Trie is
	type Element_Type is record
		A: Character;
		B: Integer;
	end record;
	function "=" (Left, Right : Element_Type) return Boolean;

	package Trie is new Ada.Containers.Multiway_Trees (Element_Type=>Element_Type, "="=>"=");
	use Trie;	

	function Find_String (T: Tree; Input: String) return Integer;
	function Add_String (T: in out Tree; Input: String; Address: Integer) return Boolean;
	function Find_Immediate_Child (Parent: Cursor; Element: Element_Type) return Cursor;
	function Find_Move_Immediate_Child (Parent: in out Cursor; Element: Element_Type) return Boolean;
	
end Improved_Trie;