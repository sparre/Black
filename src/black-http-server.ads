with
  Black.HTTP.Request;
generic
   with procedure Handler (Request : in out Black.HTTP.Request.Instance);
package Black.HTTP.Server is
   type TCP_Ports is range 0 .. 2 ** 16 - 1;

   procedure Start (Listen_To : in TCP_Ports := 80);
   procedure Stop;
end Black.HTTP.Server;
