with pp;
use pp;

package body pkg1 is

    procedure pr is
    begin
        null;
    end pr;

    function fb (i : Integer) return Boolean is
    begin
      return False or (false and true);
    end fb;
end pkg1;
