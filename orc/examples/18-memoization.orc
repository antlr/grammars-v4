-- Memoization (reference, p.35)
val res = Ref()
val s = Semaphore(1)
def memo() =
    val z = res? | s.acquire() >> res := f() >> stop
    z

memo()
