    UPDATE cycling.cyclist_name
    SET comments ='Rides hard, gets along with others, a real winner'
    WHERE id = fb372533-eb95-4bb4-8685-6ef61e994caa IF EXISTS;

    UPDATE cycling.cyclists
    SET firstname = 'Marianne',
    lastname = 'VOS'
    WHERE id = 88b8fd18-b1ed-4e96-bf79-4280797cba80;
    UPDATE cycling.cyclists
    SET firstname = 'Anna', lastname = 'VAN DER BREGGEN' WHERE id = e7cd5752-bc0d-4157-a80f-7523add8dbcd;


    UPDATE cycling.upcoming_calendar 
    SET events = ['Tour de France'] + events WHERE year=2015 AND month=06;



    UPDATE users
    SET state = 'TX'
    WHERE user_uuid
    IN (88b8fd18-b1ed-4e96-bf79-4280797cba80,
        06a8913c-c0d6-477c-937d-6c1b69a95d43,
        bc108776-7cb5-477f-917d-869c12dfffa8);

    UPDATE cyclist.cyclist_career_teams SET teams = teams + {'Team DSB - Ballast Nedam'} WHERE id = 88b8fd18-b1ed-4e96-bf79-4280797cba80;

    UPDATE cyclist.cyclist_career_teams SET teams = teams - {'WOMBATS'} WHERE id = 88b8fd18-b1ed-4e96-bf79-4280797cba80;

    UPDATE cyclist.cyclist_career_teams SET teams = {} WHERE id = 88b8fd18-b1ed-4e96-bf79-4280797cba80;