with
  Actual_Task_Type;
package body Task_Interface is
   function Create (ID : in Positive) return Class is
   begin
      return Result : Actual_Task_Type.Instance (ID) do
         null;
      end return;
   end Create;
end Task_Interface;
