fn main(){
    match x as u32 {
        0 => println!("zero!"),
        1.. => println!("positive number!"),
    };
}