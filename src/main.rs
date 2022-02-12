#![allow(unused)]
use chess::*;
use text_io::read;
fn main() {
    let mut b = BoardState::new();
    b.draw();
    /*
    b.move_by_str("a2a4");
    b.move_by_str("a1a3");
    b.move_by_str("a3g3");
    b.move_by_str("g3g4");
    b.move_by_str("g4b4");
    b.move_by_str("a4a5");
    b.move_by_str("a5a6");
    b.move_by_str("b8a6");
    b.move_by_str("d2d3");
    b.move_by_str("c1d2");
    b.move_by_str("e5e5");
    b.move_by_str("d8e7");*/

    //b.move_by_str("a6c7");

    /*b.move_by_str("e7e5");
    b.move_by_str("g1f3");
    b.move_by_str("b8b6");*/

    loop {
        let input: String = read!();
        b.move_by_str(input.as_str());
        b.draw();
    }
}
