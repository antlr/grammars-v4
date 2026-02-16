with GenPack;
pragma Elaborate(GenPack);

package Pragma_Test is

    pragma Pure;

    type Count is range 0 .. 100;

    pragma Pack(Count);

end Pragma_Test;
