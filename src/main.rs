#![allow(unused)]
use chess::*;
use text_io::read;
fn main() {
    let mut b = Board::new();
    b.draw();
    b.move_by_str("e2e4");
    b.move_by_str("e7e5");
    b.move_by_str("g1f3");
    b.move_by_str("b8c6");
    /*loop {
        let input: String = read!();
        b.move_by_str(input.as_str());
    }*/
}
