with
  Black.HTTP.Request;
generic
   with procedure Handler (Request : in out Black.HTTP.Request.Instance);
package Black.HTTP.Server is
   procedure Start (Listen_To : in TCP_Ports := 80);
   procedure Stop;
end Black.HTTP.Server;
