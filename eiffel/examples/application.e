note
    description : "root class of the application"
    date        : "$Date$"
    revision    : "$Revision$"

class
   APPLICATION

inherit
   ARGUMENTS_32

create
   make

feature {NONE} -- Initialization

   make
       -- Run application.
   do
        -- Add your code here
        print ("Hello Eiffel World!%N")
   end
end