use std::io::{self, Write};


pub fn readint() -> i64 {
    print!("input an integer: ");
    io::stdout().flush().expect("Faild to flush!");
    let mut v = String::new();
    io::stdin().read_line(&mut v)
        .expect("Failed to read line"); 
    return v.trim().parse().expect("Not an integer!");
}

/// use to check whether a variable is valid
pub fn is_digit(s: &str) -> bool {
    for c in s.chars() {
        if !c.is_digit(10) {
            return false;
        }
    }
    return true;
}


pub fn gensym() -> String {
    use uuid::Uuid;
    return Uuid::new_v4().to_string();
}


// --------------------- Macro ------------------------
#[macro_export]
macro_rules! hashmap {
    ( $( $key:expr => $val:expr ),* ) => {
        {
            let mut map = std::collections::HashMap::new();
            $( map.insert( $key, $val ); )*
            map
        }
    };
}

#[macro_export]
macro_rules! string {
    ( $x:expr ) => {
        {
            String::from($x)
        }
    };
}