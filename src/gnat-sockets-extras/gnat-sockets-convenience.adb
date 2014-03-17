--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

package body GNAT.Sockets.Convenience is
   function To_IP_Address (Host : in String) return IP_Address_Type is
      function Is_An_IP_Address (Host : in String) return Boolean is
      begin
         if Host'Length < 7 or Host'Length > 15 Then
            return False;
         else
            for Index in Host'Range loop
               case Host (Index) is
                  when '.' | '0' .. '9' =>
                     null;
                  when others =>
                     return False;
               end case;
            end loop;
            return True;
         end if;
      end Is_An_IP_Address;
   begin
      if Is_An_IP_Address (Host) then
         return Inet_Addr (Host);
      else
         return Addresses (Get_Host_By_Name (Host), 1);
      end if;
   end To_IP_Address;

   function Make_Server (Port         : in Port_Type;
                         Mode         : in Mode_Type := Socket_Stream;
                         Queue_Length : in Positive := 15) return Socket_Type is
   begin
      return Server : Socket_Type do
         Create_Socket (Socket => Server,
			Mode   => Mode);
	 Set_Socket_Option (Socket => Server,
			    Option => (Name    => Reuse_Address,
				       Enabled => True));
	 Bind_Socket (Socket  => Server,
		      Address => (Family => Family_Inet,
				  Addr   => Any_Inet_Address,
				  Port   => Port));

	 if Mode = Socket_Stream then
	    Listen_Socket (Socket => Server,
			   Length => Queue_Length);
	 end if;
      end return;
   end Make_Server;

   function Connect_To_Server (Host : in String;
                               Port : in Port_Type) return Socket_Type is
   begin
      return Client : Socket_Type do
         Create_Socket (Socket => Client);
	 Set_Socket_Option (Socket => Client,
			    Option => (Name    => Reuse_Address,
				       Enabled => True));
	 Connect_Socket (Socket => Client,
			 Server => (Family => Family_Inet,
				    Addr   => To_IP_Address (Host),
				    Port   => Port));
      end return;
   end Connect_To_Server;
end GNAT.Sockets.Convenience;
