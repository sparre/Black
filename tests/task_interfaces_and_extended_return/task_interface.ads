package Task_Interface is
   type Instance is task interface;
   subtype Class is Instance'Class;

   procedure Start (Item : in out Instance) is abstract;

   function Create (ID : in Positive) return Class;
end Task_Interface;
