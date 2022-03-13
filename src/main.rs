#![allow(unused)]
use chess::*;
use colored::Colorize;
use text_io::read;

fn main() {
    let mut b = BoardState::new();
    b.draw(true);
    let mut count = 0;
    loop {
        count += 1;
        /*if count > 40 {
            break;
        }*/
        println!("-- ENTER NEXT MOVE --");
        println!(
            "WHITE CURRENTLY IN CHECK: {}",
            b.check(PieceColor::White).to_string().red()
        );
        let input: String = read!();
        match b.move_by_str(input.as_str()) {
            Ok(()) => {
                println!("Please let me think :)");
                println!(
                    "BLACK IN CHECK (before black's move): {}",
                    b.check(PieceColor::Black).to_string().red()
                );
                let m = b.min_max(4);
                b.make(m);
                b.draw(true);
                /*println!("Please let me think :)");
                let m = b.min_max(4);
                b.make(m);
                b.draw(true);*/
            }
            Err(e) => eprintln!("{}", e.to_string().red()),
        }
    }
}
