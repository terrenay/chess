#![allow(unused)]
use chess::*;
use colored::Colorize;
use text_io::read;
mod evaluation;
mod zobrist;

fn main() {
    let mut b = BoardState::new();
    b.draw(true);
    let mut count = 0;
    println!("e: egine, h: play");
    let game_mode: String = read!();
    let zobrist = zobrist::ZobristState::from_board_state(&b);
    println!("{}", zobrist.hash);
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
                let m = b.iterative_deepening(5000).0;
                if m.is_none() {
                    return;
                }
                let m = m.unwrap();
                //println!("white makes move {} to {}", m.from, m.to);
                b.make(m);
                b.draw(true);
                println!("-- BLACK TO MOVE --");
                let m = b.iterative_deepening(4000).0;
                if m.is_none() {
                    return;
                }
                b.make(m.unwrap());
                b.draw(true);
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
                        println!(
                            "Hash after white's move: {}",
                            zobrist::ZobristState::from_board_state(&b).hash
                        );
                        println!("Please let me think :)");
                        println!(
                            "BLACK IN CHECK (before black's move): {}",
                            b.check(PieceColor::Black).to_string().red()
                        );
                        let m = b.iterative_deepening(1000).0;
                        if m.is_none() {
                            return;
                        }
                        b.make(m.unwrap());
                        b.draw(true);
                        println!(
                            "Hash after black's move: {}",
                            zobrist::ZobristState::from_board_state(&b).hash
                        );
                    }
                    Err(e) => eprintln!("{}", e.to_string().red()),
                }
            }
        }
        _ => (),
    }
}
