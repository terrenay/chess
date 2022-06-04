#![allow(unused)]
use std::thread::sleep;

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
                // let m = b.negamax_standalone(6).0;
                let m = b.iterative_deepening_nega(None, Some(4)).1;
                if m.is_none() {
                    b.print_all_moves();
                    return;
                }
                let m = m.unwrap();
                b.make(&m);
                b.draw(true);
                //println!("{:?}", b.hash_history);
                println!("-- BLACK TO MOVE --");
                //let m = b.minimax_standalone(6).0;
                // let m = b.negamax_standalone(6).0;
                let m = b.iterative_deepening_nega(None, Some(4)).1;
                if m.is_none() {
                    b.print_all_moves();
                    return;
                }
                let m = m.unwrap();
                b.make(&m);
                b.draw(true);
                //println!("{:?}", b.hash_history);
            }
        }
        "h" => {
            println!("Play as black (b) or white (w)?");
            let player_color: String = read!();
            //human
            match player_color.as_str() {
                "w" => {
                    loop {
                        count += 1;
                        match evaluation::end_of_game(&mut b, None).0 {
                            Some(0) => {
                                println!("Draw");
                                b.print_all_moves();
                                return;
                            }
                            Some(_) => {
                                println!("You lost. Checkmate for black.");
                                b.print_all_moves();
                                return;
                            }
                            None => (),
                        }
                        println!("-- ENTER NEXT MOVE --");
                        println!(
                            "WHITE CURRENTLY IN CHECK: {}",
                            b.check(PieceColor::White).to_string().red()
                        );
                        todo!("wenn schwarz mate findet macht es das was weiss am schnellsetn mate gibt");
                        let input: String = read!();
                        match b.move_by_str(input.as_str()) {
                            Ok(()) => {
                                println!("Please let me think :)");
                                println!(
                                    "BLACK IN CHECK (before black's move): {}",
                                    b.check(PieceColor::Black).to_string().red()
                                );
                                let m = b.iterative_deepening_nega(Some(2000), None).1;
                                if m.is_none() {
                                    b.print_all_moves();
                                    return;
                                }
                                let m = m.unwrap();
                                b.make(&m);
                                b.draw(true);
                            }
                            Err(e) => eprintln!("{}", e.to_string().red()),
                        }
                    }
                }
                "b" => {
                    println!("Please let me think :)");
                    let m = b.iterative_deepening_nega(Some(2000), None).1;
                    if m.is_none() {
                        b.print_all_moves();
                        return;
                    }
                    let m = m.unwrap();
                    b.make(&m);
                    b.draw(true);
                    loop {
                        count += 1;
                        match evaluation::end_of_game(&mut b, None).0 {
                            Some(0) => {
                                println!("Draw");
                                b.print_all_moves();
                                return;
                            }
                            Some(_) => {
                                println!("You lost. Checkmate for black.");
                                b.print_all_moves();
                                return;
                            }
                            None => (),
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
                                let m = b.iterative_deepening_nega(Some(2000), None).1;
                                if m.is_none() {
                                    b.print_all_moves();
                                    return;
                                }
                                let m = m.unwrap();
                                b.make(&m);
                                b.draw(true);
                            }
                            Err(e) => eprintln!("{}", e.to_string().red()),
                        }
                    }
                }
                _ => (),
            }
        }
        _ => (),
    }
}
