(**************************************************************************
 *
 * This program maintains a small work schedule covering the hours 8-6.
 * It processes the following commands:
 *
 * sched employee startday endday starthour endhour
 *        The  employee is added to the schedule in the range of days and
 *        hours  indicated.
 *
 * clear startday endday starthour endhour
 *        The  part  of the schedule indicated is cleared  any assignment
 *        of an employee during those hours is removed. 
 *
 * unsched employee
 *        Remove  the  employee  from all places in the schedule to which
 *        s/he has been assigned.
 *
 * print
 *        Print  the  schedule. Show a table with days along the top, and
 *        hours  down  the  side,  giving  the scheduled employee in each
 *        position.
 *
 * total employee
 *        Print  the  total hours for which the employee is scheduled. If
 *        the employee does not appear in the table, the total is 0.
 *
 * quit
 *        Terminate the program.
 **************************************************************************)

PROGRAM a1 (input,output);
    USES dayio;

    CONST 
        { Open positions in the schedule. }
        NotScheduled = '        ';

        { Max length of an employee name. }
        EmployeeMaxLen = 8;

        { Hours in a day. }
        FirstHour = 8;
        LastHour = 17;          { 5:00 PM in 24-hour time }
        PastLastHour = 18;      { One past, for while loops. }

        { How much room to allow for each day in the table. }
        TableDayWidth = 9;
    TYPE 
        { The employee name type. }
        EmployeeType = string[EmployeeMaxLen];

        { The type of the schedule ARRAY. }
        { HourType = FirstHour..LastHour; }
        HourType = 8..17;
        ScheduleType = ARRAY [HourType, DayType] OF EmployeeType;
        { HourScanType = FirstHour..PastLastHour; }
        HourScanType = 8..18;

    (***********************************************************************
     * Procedure to read the next non-blank.  It skips leading blanks, then
     * reads the string up to the first blank or eoln.
     ***********************************************************************)
    PROCEDURE ReadString(VAR Str: string);
        VAR
            Ch: char;
        BEGIN
            Ch := ' ';
            WHILE (Ch = ' ') AND NOT eoln DO 
                read(Ch);

            IF Ch = ' ' THEN
                { There is no command on this line. }
                Str := ''
            ELSE
                BEGIN 
                    { Read the beast. }
                    Str := '';
                    WHILE (Ch <> ' ') AND NOT eoln DO
                        BEGIN
                            Str := Str + Ch;
                            read(Ch)
                        END;

                    IF Ch <> ' ' THEN
                        { Command ended at eoln. }
                        Str := Str + Ch
                END
        END; { ReadString }

    (***********************************************************************
     * Procedure to read the arguments held in common by the sched 
     * clear commands.  Returns them through the arguments.  If there
     * is some error, that is reported through the argument error.
     *  Precondition: Following the read pointer, the input contains
     *    two days of the week, then two integers.  If all days are present and
     *    correct, the integers must be present and correct.
     *  Postcondition: If both strings are recognized day names,
     *    they are read, and the integers are read as well, and their values
     *    are loaded into StartDay, EndDay, StartHour, and EndHour, and Error
     *    is set to false.  The hours are mapped to 24-hour clock time under
     *    the rule that hours less than 6 are PM, and others are AM.  If a day
     *    is missing or not recognized, the rest of the input line is
     *    discarded, and Error is set to true.  If there is extra information
     *    on the line, it is discared.  The read pointer is left at the start
     *    of the following line.
     ***********************************************************************)
    PROCEDURE ReadSchedClrArgs(
            VAR StartDay, EndDay: DayType;      { Input days. }
            VAR StartHour, EndHour: HourType;   { Input hour range. }
            VAR Error: boolean);                { Input error indicator.}
        VAR
            InputHour: integer;                 { Input hour value. }

        { Map time to 24-hours based on the AM/PM rules. }
        FUNCTION MapTo24(Hour: integer): HourType;
            CONST
                { AM/PM time cut-off. }
                LastPM = 5;
            BEGIN
                IF Hour <= LastPM THEN
                    MapTo24 := Hour + 12
                ELSE
                    MapTo24 := Hour
            END;

        BEGIN { ReadSchedClrArgs }
            { Read the days. }
            ReadDay(input, StartDay);
            ReadDay(input, EndDay);

            { See if they both worked. }
            IF (StartDay <> BadDay) AND (EndDay <> BadDay) THEN 
                BEGIN
                    { It worked.  Read the hours. }
                    read(InputHour);
                    StartHour := MapTo24(InputHour);
                    read(InputHour);
                    EndHour := MapTo24(InputHour);

                    { Report success }
                    Error := FALSE 
                END
            ELSE
                (* Something went wrong, seriously wrong. *)
                Error := TRUE;

            (* We're done with this line. *)
            readln
        END; { ReadSchedClrArgs }

    {****************************************************************
     * PROCEDURE to print headers of each day.
     *  Precondition: None.
     *  Postcondition: A header line with the days of the week has
     *    been printed.  The 
     ****************************************************************}
    PROCEDURE WriteDaysHeader;
        CONST

            { How many spaces to move over before printing days-of
              the week header. }
            DaysHeadMoveOver = 6;

            { How much room to assume is needed by each day string. }
            AllowForDay = 3;
        VAR
            Day: DayType;
        BEGIN
            write(' ': DaysHeadMoveOver);

            FOR Day := Sun TO Sat DO
                BEGIN
                    write('[ ');
                    WriteDay(output, Day);
                    write(' ]', ' ': TableDayWidth - AllowForDay - 4)
                END;
            writeln
        END; { WriteDaysHeader }

    {****************************************************************
     * Function that tells if a pending schedule is legal.
     * Its arguments are those of sched, excluding the employee name.
     *  Precondition: FirstHour and LastHour are in range.
     *  Postcondition: If the indicated area of the schedule contains
     *    blanks in each entry, then return true, else false.
     *  Note: Schedule is sent by VAR for efficiency -- it is not
     *    changed.
     ****************************************************************}
    FUNCTION SchedLegal(
            VAR Schedule: ScheduleType;     { Schedule to check. }
                StartDay, EndDay: DayType;  { Days in question. }
                FirstHour, LastHour:        { Hours in question. }
                        HourType): boolean;
        VAR
            ConflictFound: boolean;         { Tell if one found. }
            DayScan: DayType;               { Go through the days. }
            HourScan: HourScanType;         { Go through the hours. }
        BEGIN
            { Scan the days. }
            DayScan := StartDay;
            ConflictFound := FALSE;
            REPEAT
                { For this day, scan the times. }
                HourScan := FirstHour;
                WHILE NOT ConflictFound AND
                                (HourScan <= LastHour) DO BEGIN
                    { Conflict? }
                    ConflictFound :=
                            Schedule[HourScan, DayScan] <> NotScheduled;

                    { Next one. }
                    HourScan := HourScan + 1
                END;

                { Next Day. }
                DayScan := succ(DayScan)
            UNTIL ConflictFound or (DayScan > EndDay);

            { And the answer is.. }
            SchedLegal := not ConflictFound
        END; { SchedLegal }

    {****************************************************************
     * This takes care of most of the work of the clear and sched
     * commands.  Its arguments are those of sched, with blanks in
     * Employee for the clear.  It places this name in each indicated
     * postion.
     *  Precondition: FirstHour and LastHour are in range.
     *  Postcondition: The area of the schedule is changed to show
     *    the indicated employee.
     *  Note: This will replace any old entry, so the sched command
     *    should call SchedLegal above to make sure the operation
     *    is legal before calling this routine.
     ****************************************************************}
    PROCEDURE SetSchedPart(
            VAR Schedule: ScheduleType;     { Set me! Set me! }
                Employee: EmployeeType;     { Who gets to work. }
                StartDay, EndDay: DayType;  { Days in question. }
                FirstHour, LastHour:        { Hours in question. }
                                HourType);
        VAR
            DayScan: DayType;               { Go through the days. }
            HourScan: HourType;             { Go through the hours. }
        BEGIN
            for DayScan := StartDay to EndDay do
                for HourScan := FirstHour to LastHour do
                    Schedule[HourScan, DayScan] := Employee
        END; { SetSchedPart }

    {****************************************************************
     * Perform the sched command.
     *  Precondition: The read pointer is followed by the arguments 
     *    for the sched command.
     *  Postcondition: The arguments have been read and echoed, and the
     *    read pointer is on the next line.  The sched command has been
     *    performed with appropriate messages.
     * Note: DayMap is passed by VAR for efficiency -- it is not
     *    changed.
     ****************************************************************}
    PROCEDURE DoSched(
            VAR Schedule: ScheduleType);    { Change this. }
        VAR
            Employee: EmployeeType;         { Input employee name. }
            StartDay, EndDay: DayType;      { Input days. }
            StartHour, EndHour: HourType;   { Input hour range. }
            Error: boolean;                 { Input error indicator.}
        BEGIN
            { Read the employee name }
            ReadString(Employee);

            { Read all the other arguments, and recieve error 
               indication. }
            ReadSchedClrArgs(StartDay, EndDay, StartHour, EndHour, Error);

            { For errors, let 'em know.  Otherwise, do it. }
            IF Error THEN
                writeln('*** Un-recognized day code.  ',
                    'Command not performed. ***')
            ELSE 
                { See if the scheduling is legal. }
                IF SchedLegal(Schedule, StartDay, EndDay,
                                        StartHour, EndHour) THEN
                    BEGIN
                        { Legal.  Do it and admit it. }
                        SetSchedPart(Schedule, Employee,
                                StartDay, EndDay, StartHour, EndHour);
                        writeln('>>> ', Employee, ' scheduled. <<<')
                    END
                ELSE 
                    { Not legal. }
                    writeln('*** Conflicts with existing schedule.  ',
                        'Command not performed. ***')
        END; { DoSched }

    {****************************************************************
     * Perform the clear command.
     *  Precondition: The read pointer is followed by the arguments 
     *    for the clear command.
     *  Postcondition: The arguments have been read and echoed, and the
     *    read pointer is on the next line.  The clear command has been
     *    performed with appropriate messages.
     * Note: DayMap is passed by VAR for efficiency -- it is not
     *    changed.
     ****************************************************************}
    PROCEDURE DoClear(
            VAR Schedule: ScheduleType);    { Change this. }
        VAR
            StartDay, EndDay: DayType;      { Input days. }
            StartHour, EndHour: HourType;   { Input hour range. }
            Error: boolean;                 { Input error indicator.}
        BEGIN
            { Read the arguments, and recieve error indication. }
            ReadSchedClrArgs(StartDay, EndDay, StartHour, EndHour, Error);

            { For errors, let 'em know.  Otherwise, do it. }
            IF Error THEN
                writeln('*** Un-recognized day code.  ',
                    'Command not performed. ***')
            ELSE 
                BEGIN
                    SetSchedPart(Schedule, NotScheduled, StartDay, EndDay,
                        StartHour, EndHour);
                    writeln('>>> Clear performed. <<<');
                END { DoClear }
        END;

    {****************************************************************
     * Peform the unsched command.
     *  Precondition: The read pointer is followed by an employee 
     *    name.
     *  Postcondition: The argument has been read and echoed, and the
     *    read pointer is on the next line.  The employee read has been
     *    removed from Schedule.
     ****************************************************************}
    PROCEDURE DoUnsched(
            VAR Schedule: ScheduleType);        { Remove from. }
        VAR
            Employee: EmployeeType;             { To remove. }
            Day: DayType;                       { Column scanner. }
            Hour: integer;                      { Row scanner. }
            Found: boolean;                     { Presence indicator }
        BEGIN
            { Read the employee. }
            readln(Employee);

            { Remove! Remove! }
            Found := FALSE;
            FOR Day := Sun TO Sat DO
                FOR Hour := FirstHour TO LastHour DO
                    IF Schedule[Hour, Day] = Employee THEN 
                        BEGIN
                            { Remove. }
                            Schedule[Hour, Day] := NotScheduled;

                            { Note. }
                            Found := TRUE 
                        END;

            { Warn if not found. Else just state. }
            IF Found THEN 
                write('>>> ', Employee, ' removed from schedule. <<<')
            ELSE
                write('>>> ', Employee, 
                                    ' was not on the schedule. <<<')
        END; { DoUnsched }

    {****************************************************************
     * Peform the print command.
     *  Precondition: None.
     *  Postcondition: Schedule has been printed to output.
     ****************************************************************}
    PROCEDURE DoPrint(
            VAR Schedule: ScheduleType);        { Print me. }
        VAR
            Hour: HourType;                     { Hour scan. }
            Day: DayType;                       { Day scan. }

        { Map from 24-hour time to 12-hour time.  Arguments less than
          13 are simply returned, arguments greater than 12 are 
          reduced by 12 and returned. }
        FUNCTION Map24to12(HourType: HourType): integer;
            BEGIN
                IF Hour < 13 THEN
                    Map24to12 := Hour
                ELSE
                    Map24to12 := Hour - 12
            END;
        BEGIN
            readln;
            WriteDaysHeader;

            FOR Hour := FirstHour TO LastHour DO
                BEGIN
                    write(Map24to12(Hour):2, ':00 ');
                    FOR Day := Sun TO Sat DO
                        write(Schedule[Hour, Day], 
                            ' ': TableDayWidth - length(Schedule[Hour, Day]));
                    writeln
                END
        END;

    {****************************************************************
     * Peform the total command.
     *  Precondition: The read pointer is followed by an employee 
     *    name.
     *  Postcondition: The argument has been read and echoed, and the
     *    read pointer is on the next line.  The total scheduled hours
     *    for the employee read has been printed.
     ****************************************************************}
    PROCEDURE DoTotal(
            VAR Schedule: ScheduleType);        { The schedule. }
        VAR
            Employee: EmployeeType;             { To remove. }
            Day: DayType;                       { Column scanner. }
            Hour: integer;                      { Row scanner. }
            Total: integer;                     { Total intgers. }
        BEGIN
            { Read the employee. }
            readln(Employee);

            { Do the sum. }
            Total := 0;
            FOR Day := Sun TO Sat DO
                FOR Hour := FirstHour TO LastHour DO
                    IF Schedule[Hour, Day] = Employee THEN
                        Total := Total + 1;

            { Write the total. }
            writeln('>>> ', Employee,
                ' is scheduled for ', Total:1, ' hours. <<<<')
        END; { DoTotal }

    {*****************************************************************
     * Main line.
     *****************************************************************}

    VAR
        { The schedule. }
        Schedule: ScheduleType;

        { Main loop continue flag. }
        KeepRunning: boolean;

        { Command input local to main. }
        Command: string;

    BEGIN
        { Clear the schedule. }
        SetSchedPart(Schedule, NotScheduled, Sun, Sat, FirstHour, LastHour);
 
        { Do the commands. }
        write('==> ');
        ReadString(Command);
        KeepRunning := TRUE;
        WHILE KeepRunning DO
            BEGIN
                IF Command = 'sched' THEN 
                    DoSched(Schedule)
                ELSE IF Command = 'clear' THEN
                    DoClear(Schedule)
                ELSE IF Command = 'unsched' THEN
                    DoUnsched(Schedule)
                ELSE IF Command = 'print' THEN
                    DoPrint(Schedule)
                ELSE IF Command = 'total' THEN
                    DoTotal(Schedule)
                ELSE IF Command = 'quit' THEN 
                    BEGIN
                        writeln;
                        writeln('>>> Program terminating. <<<');
                        KeepRunning := FALSE
                    END
                ELSE
                    { Command not recognized. }
                    BEGIN
                        readln;
                        writeln;
                        writeln('*** Command ', Command, 
                                                    ' not recognized. ***');
                    END;

                { Go to a new page for next'n. }
                write('==> ');
                ReadString(Command)
            END
    END.
