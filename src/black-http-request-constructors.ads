with
  GNAT.Sockets;
with
  Black.HTTP.Request;
package Black.HTTP.Request.Constructors is
   function From (Connection : in GNAT.Sockets.Socket_Type)
                 return Black.HTTP.Request.Instance;
end Black.HTTP.Request.Constructors;
