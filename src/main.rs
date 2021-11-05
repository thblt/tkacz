use std::path::Path;

use tkacz::*;

fn main() {
    println!("This is Tkacz {}!", tkacz::VERSION);
    match Store::init(Path::new("/home/thblt/.tkcz")) {
        Ok(_) => println!("Got a store"),
        Err(e) => println!("Cannot create store: {}", e),
    };
}
