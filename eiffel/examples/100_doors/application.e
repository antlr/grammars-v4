note
    description: "100 Doors problem"
    date: "08-JUL-2015"
    revision: "1.1"

class
    APPLICATION

create
    make

feature {NONE} -- Initialization

    make
            -- Main application routine.
        do
            initialize_closed_doors
            toggle_doors
            output_door_states
        end

feature -- Access

    doors: ARRAYED_LIST [DOOR]
            -- A set of doors (self-initialized to capacity of `max_door_count').
        attribute
            create Result.make (max_door_count)
        end

feature -- Basic Operations

    initialize_closed_doors
            -- Initialize all `doors'.
        do
            across min_door_count |..| max_door_count as ic_address_list loop
                doors.extend (create {DOOR}.make_closed (ic_address_list.item))
            end
        ensure
            has_all_closed_doors: across doors as ic_doors_list all not ic_doors_list.item.is_open end
        end

    toggle_doors
            -- Toggle all `doors'.
        do
            across min_door_count |..| max_door_count as ic_addresses_list loop
                across doors as ic_doors_list loop
                    if is_door_to_toggle (ic_doors_list.item.address, ic_addresses_list.item) then
                        ic_doors_list.item.toggle_door
                    end
                end
            end
        end

    output_door_states
            -- Output the state of all `doors'.
        do
            doors.do_all (agent door_state_out)
        end

feature -- Status Report

    is_door_to_toggle (a_door_address, a_index_address: like {DOOR}.address): BOOLEAN
            -- Is the door at `a_door_address' needing to be toggled, when compared to `a_index_address'?
        do
            Result := a_door_address \\ a_index_address = 0
        ensure
            only_modulus_zero: Result = (a_door_address \\ a_index_address = 0)
        end

feature -- Outputs

    door_state_out (a_door: DOOR)
            -- Output the state of `a_door'.
        do
            print ("Door " + a_door.address.out + " is ")
            if a_door.is_open then
                print ("open.")
            else
                print ("closed.")
            end
            io.new_line
        end

feature {DOOR} -- Constants

    min_door_count: INTEGER = 1
            -- Minimum number of doors.

    max_door_count: INTEGER = 100
            -- Maximum number of doors.

end