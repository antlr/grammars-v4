fn main(){
    for c in self.bytes() {
        match c {
            Err(e) => return Err(e),
            Ok(0 | 3 | 4) => return Ok(None),
            Ok(0x7f) => { buf.pop(); }
            Ok(b'\n' | b'\r') => break,
            Ok(c) => buf.push(c),
        }
    }
}