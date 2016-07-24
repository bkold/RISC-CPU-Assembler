package body Improved_Trie is

	function Find_Move_Immediate_Child (Parent: in out Cursor; Element: Element_Type) return Boolean is
		Tree_Leaf: Trie.Cursor;
		begin
			Tree_Leaf:= Trie.First_Child(Parent);
			while Trie.Has_Element(Tree_Leaf) loop	
				if Trie.Element(Tree_Leaf) = Element then
					Parent:=Tree_Leaf;
					return True;
				else
					Trie.Next_Sibling(Tree_Leaf);
				end if;
			end loop;
			return False;

	end Find_Move_Immediate_Child;

	function Find_Immediate_Child (Parent: Cursor; Element: Element_Type) return Cursor is
		Tree_Leaf: Cursor;
		begin
			Tree_Leaf:= First_Child(Parent);
			while Has_Element(Tree_Leaf) loop						
				if Trie.Element(Tree_Leaf) = Element then
					return Tree_Leaf;
				else
					Next_Sibling(Tree_Leaf);
				end if;
			end loop;
			return No_Element;

	end Find_Immediate_Child;

	function Add_String (T: in out Tree; Input: String; Address: Integer) return Boolean is
		Tree_Leaf: Cursor:= T.Root;
		Test_Element: Element_Type;
		begin
			Test_Element.B:= -1;
			--Ada.Text_IO.Put_Line("----");
			for I in Input'First..Input'Last loop
				Test_Element.A:= Input(I);
				--Ada.Text_IO.Put_Line(Character'Image(Test_Element.A));
				if Find_Move_Immediate_Child(Tree_Leaf, Test_Element) = False then
					--Ada.Text_IO.Put_Line("Appending " & Character'Image(Test_Element.A));
					Append_Child(T, Tree_Leaf, Test_Element);
					Tree_Leaf:= Last_Child(Tree_Leaf);
				end if;	
			end loop;
			Test_Element.A:= Input(Input'Last);
			Test_Element.B:= Address;
			if Element(Tree_Leaf).B > -1 then
				return False; --Duplicate string
			end if;
			Replace_Element(T, Tree_Leaf, Test_Element);
			return True;

	end Add_String;

	function Find_String (T: Tree; Input: String) return Integer is
		Tree_Leaf: Cursor:= T.Root;
		Test_Element: Element_Type;
		begin
			for I in 1..Input'Length loop
				Test_Element.A:= Input(I);
				if Find_Move_Immediate_Child(Tree_Leaf, Test_Element) = False then
					return -1;
				end if;	
			end loop;
			return Element(Tree_Leaf).B;

	end Find_String;

end Improved_Trie;