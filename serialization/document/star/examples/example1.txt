# Comments can be at the start of a file
# Should this be file level or a comment on 'data_setA' datablock?

data_setA

# Is this a data block comment or for the following key-value pair

    _location   'New Mexico'

    save_observation1
        
        _date   # what about comments inside pairs? 
        2020-07-01

        loop_

            _sampleID   _height_millimeters   # Or inside loops
            1           6.3                   # Within columns or rows
            2           2.5

# Is this the last comment of the previous block or a comment about the next one?

global_

    _max_height 6.3

data_setB

    _location   California
    
    save_observation1
    
        _date       2020-09-15

        loop_

        _sampleID   _height_millimeters
        1           9.3
        
    save_observation2
    
        _date       2020-10-15

        loop_

        _sampleID   _height_millimeters
        1           9.9

global_

_max_height 9.9