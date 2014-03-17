--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

package GNAT.Sockets.Convenience is
   subtype IP_Address_Type is Inet_Addr_Type;

   function To_IP_Address (Host : in String) return IP_Address_Type;

   function Make_Server (Port         : in Port_Type;
                         Mode         : in Mode_Type := Socket_Stream;
                         Queue_Length : in Positive := 15) return Socket_Type;

   function Connect_To_Server (Host : in String;
                               Port : in Port_Type) return Socket_Type;
end GNAT.Sockets.Convenience;
