use std::num::ParseIntError;
use std::io::{self, Write};


pub fn readint() -> Result<i64, ParseIntError> {
    print!("input an integer: ");
    io::stdout().flush().expect("Faild to flush!");
    let mut v = String::new();
    io::stdin().read_line(&mut v)
        .expect("Failed to read line"); 
    return v.trim().parse();
}