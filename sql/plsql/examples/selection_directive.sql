begin
    $if true $then
        dbms_output.put_line(1);
    $else
    $end

    $if false $then
    $else
        dbms_output.put_line(2);
    $end

    $if false $then
    $elsif true $then
        dbms_output.put_line(3);
    $else
    $end

    $if false $then
        dbms_output.put_line('if');
    $elsif true $then
        dbms_output.put_line('else if');
    $else
        dbms_output.put_line('else');
    $end

    $if true $then
    $end

    $if true $then
    $else
    $end

    $if false $then
    $elsif true $then
    $end

    $if false $then
    $elsif true $then
    $else
    $end
end;
