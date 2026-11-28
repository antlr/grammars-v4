# break out of a loop early, and skip even numbers with next
integer i
for (i = 1; i <= 20; i = i + 1) {
    if (i > 10) {
        break
    }
    if (i == 2 * (i / 2)) {
        next
    }
    print *, i
}
