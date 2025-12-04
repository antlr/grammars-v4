note
    description: "A door with an address and an open or closed state."
    date: "08-JUL-2015"
    revision: "1.1"

class
    DOOR

create
    make_closed,
    make

feature {NONE} -- initialization

    make_closed (a_address: INTEGER)
            -- Initialize Current {DOOR} at `a_address' and state of `Is_closed'.
        require
            positive: a_address >= {APPLICATION}.min_door_count and a_address >= Min_door_count
        do
            make (a_address, Is_closed)
        ensure
            closed: is_open = Is_closed
        end

    make (a_address: INTEGER; a_status: BOOLEAN)
            -- Initialize Current {DOOR} with `a_address' and `a_status', denoting position and `is_open' or `Is_closed'.
        require
            positive: a_address >= {APPLICATION}.min_door_count and a_address >= Min_door_count
        do
            address := a_address
            is_open := a_status
        ensure
            address_set: address = a_address
            status_set: is_open = a_status
        end

feature -- access

    address: INTEGER
            -- `address' of Current {DOOR}.

    is_open: BOOLEAN assign set_open
            -- `is_open' (or not) status of Current {DOOR}.

feature -- Setters

    set_open (a_status: BOOLEAN)
            -- Set `status' with `a_status'
        do
            is_open := a_status
        ensure
            open_updated: is_open = a_status
        end

feature {APPLICATION} -- Basic Operations

    toggle_door
            -- Toggle Current {DOOR} from `is_open' to not `is_open'.
        do
            is_open := not is_open
        ensure
            toggled: is_open /= old is_open
        end

feature {NONE} -- Implementation: Constants

    Is_closed: BOOLEAN = False
            -- State of being not `is_open'.

    Min_door_count: INTEGER = 1
            -- Minimum door count.

invariant
    one_or_more: address >= 1
    consistency: is_open implies not Is_closed

end