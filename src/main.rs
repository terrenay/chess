#![allow(unused)]
use chess::*;
use colored::Colorize;
use text_io::read;
mod evaluation;
mod zobrist;

fn main() {
    let mut b = BoardState::new();
    b.draw(true);
    //b.negamax_standalone(3).0;
    let mut count = 0;
    println!("e: egine, h: play");
    let game_mode: String = read!();
    match game_mode.as_str() {
        "e" => {
            //engine
            loop {
                if count > 300 {
                    //avoid infinite loops
                    break;
                }
                count += 1;
                println!("-- WHITE TO MOVE --");
                //let m = b.minimax_standalone(6).0;
                let m = b.iterative_deepening(1000).0;
                if m.is_none() {
                    return;
                }
                let m = m.unwrap();
                b.make(&m);
                b.draw(true);
                //println!("{:?}", b.hash_history);
                println!("-- BLACK TO MOVE --");
                //let m = b.minimax_standalone(6).0;
                let m = b.iterative_deepening(1000).0;
                if m.is_none() {
                    return;
                }
                let m = m.unwrap();
                b.make(&m);
                b.draw(true);
                //println!("{:?}", b.hash_history);
            }
        }
        "h" => {
            //human
            loop {
                count += 1;
                if evaluation::checkmate(&mut b) {
                    println!("You lost. Checkmate for black.");
                    return;
                }
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
                        let m = b.negamax_standalone(6).0;
                        if m.is_empty() {
                            return;
                        }
                        let m = m.last().unwrap();
                        b.make(m);
                        b.draw(true);
                    }
                    Err(e) => eprintln!("{}", e.to_string().red()),
                }
            }
        }
        _ => (),
    }
}
