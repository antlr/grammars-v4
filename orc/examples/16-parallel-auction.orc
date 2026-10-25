-- Parallel Auction with Timeout (reference, p.24)
def auction([]) = 0
def auction(b : bs) =
    max(
        b.ask() | (Rwait(8000) >> 0),
        auction(bs)
    )

auction(bidders)
