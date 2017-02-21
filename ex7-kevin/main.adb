with Ada.Text_IO;
use Ada.Text_IO;

procedure Hello is
	-- package IO renames Ada.Text_IO;
	Var : Boolean := False;
	N : Integer := 30;
begin
	Var := True;
	if Var then
		Put_Line("KEK" & Integer'Image(100));
	end if;
	while N > 0 loop
		N := N - 1;
		Put_Line("OK");
	end loop;
	Put_Line("Hi!");
	if 2 > 3 then
		Put_Line("B");
	elsif 2 = 2 then
		Put_Line("A");
	else
		Put_Line("K");
	end if;
end Hello;
