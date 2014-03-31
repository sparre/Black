with
  Ahven.Framework;

package Black.Tests.Memory_Streams is
   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);
private
   procedure Storing_Strings;
   procedure HTTP_Responses;
end Black.Tests.Memory_Streams;
