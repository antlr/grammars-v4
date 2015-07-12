(*****************************************************************************
 * A simple bubble sort program.  Reads integers, one per line, and prints   *
 * them out in sorted order.  Blows up if there are more than 49.            *
 *****************************************************************************)
program Sort(input, output);
    const
        (* Max array size. *)
        MaxElts = 50;
    type
        (* Type of the element array. *)
        IntArrType = array [1..MaxElts] of Integer;

    var
        (* Indexes, exchange temp, array size. *)
        i, j, tmp, size: integer;

        (* Array of ints *)
        arr: IntArrType;

    (* Read in the integers. *)
    procedure ReadArr(var size: integer; var a: IntArrType);
        begin
            size := 1;
            while not eof do begin
                readln(a[size]);
                if not eof then 
                    size := size + 1
            end
        end;

    begin
        (* Read *)
        ReadArr(size, arr);

        (* Sort using bubble sort. *)
        for i := size - 1 downto 1 do
            for j := 1 to i do 
                if arr[j] > arr[j + 1] then begin
                    tmp := arr[j];
                    arr[j] := arr[j + 1];
                    arr[j + 1] := tmp;
                end;

        (* Print. *)
        for i := 1 to size do
            writeln(arr[i])
    end.
            