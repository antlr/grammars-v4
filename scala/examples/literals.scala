package object Literals {
    
    def integers() = {
        2;
        313;
        -1231123;
        0x61231FFF
    }
    def float() = {
        2.3
        2e3
        -23.123
        23.123e213
    } 
    def id() = {
        aaodjslkd
        `yield`
        a
        `yield a test`
    }
    def boolean() ={
        true
        false
    }
    
    def char() ={
        '2'
        '\n'
        '\"'
    } 
    
    def strings() = {
        "23\n"
        """ A 
        "quoted" e12
        MAGICAL 
        STRING """""""
    }
    
    def escapedString() {
        a"Test $id ${2 + 3}"
        multiline""" TEST
        "" ${23}
        $id TESTY
        ${2+3} TESEST
         """
    } 


    def symbols() = {
        'a
        'bb
        '+++
    }
    
}