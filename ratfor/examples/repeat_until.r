# repeat/until always executes the body at least once
integer i
i = 0
repeat {
    i = i + 1
    print *, i
} until (i >= 5)
