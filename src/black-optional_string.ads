with Ada.Strings.Unbounded;

with Black.Generic_Optional;

private
package Black.Optional_String is
  new Black.Generic_Optional (Ada.Strings.Unbounded.Unbounded_String);
