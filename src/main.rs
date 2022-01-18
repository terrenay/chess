#![allow(unused)]
use chess::*;
use text_io::read;
fn main() {
    let mut b = Board::new();
    b.draw();
    for y in 0..12 {
        for x in 0..10 {
            println!("{}, {}: {:?}", y, x, b.pseudo_legal_moves(y, x));
        }
    }

    b.move_by_str("e2e4");
    b.move_by_str("e7e5");
    b.move_by_str("g1f3");
    b.move_by_str("b8b6");
    /*loop {
        let input: String = read!();
        b.move_by_str(input.as_str());
    }*/
}
