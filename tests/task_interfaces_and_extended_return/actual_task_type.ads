with
  Task_Interface;
package Actual_Task_Type is
   task type Instance (ID : Positive) is new Task_Interface.Instance with
      entry Start;
   end Instance;
end Actual_Task_Type;
