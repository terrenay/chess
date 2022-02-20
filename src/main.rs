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
    //println!("best: {}", b.min_max(4));

    //todo: king in eval wieder auf 0 setzen!
    loop {
        println!("-- ENTER NEXT MOVE --");
        let input: String = read!();
        b.move_by_str(input.as_str());
        b.draw();
        println!("Please let me think :)");
        let m = b.min_max(4);
        b.make(m);
        b.draw();
        /*println!("All valid moves in this position:");
        for m in b.generate_moves() {
            println!("{}", m);
        }*/
    }
}
