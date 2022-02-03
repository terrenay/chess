#![allow(unused)]
use chess::*;
use text_io::read;
fn main() {
    let mut b = Board::new();
    b.draw();
    for rank in 1..=8 {
        for file in 1..=8 {
            println!(
                "{}, {}: {:?}",
                rank,
                file,
                b.pseudo_legal_moves(rank, file, MoveType::Default)
            );
        }
    }

    b.move_by_str("a2a4");
    b.move_by_str("a1a3");
    b.move_by_str("a3g3");
    b.move_by_str("g3g4");
    b.move_by_str("g4b4");
    b.move_by_str("a4a5");
    b.move_by_str("a5a6");
    b.move_by_str("b8a6");
    println!("{:?}", b.pseudo_legal_moves(6, 1, MoveType::Attack));

    //TODO: Gibt Fehler, wenn knight ganz links am Rand steht, weil padding dort nur eins
    //breit ist. Sollte ich auch auf 2 erhöhen (wie nach oben). Muss nur array size
    //anpassen und die toBoard funktion :) Gott sei dank hab ich ja jetzt alles
    //abhängig von rank und file gemacht haha
    //TODO: farbe von weissen feldern bisschen dunkler machen

    //b.move_by_str("a6c7");

    /*b.move_by_str("e7e5");
    b.move_by_str("g1f3");
    b.move_by_str("b8b6");*/
    /*loop {
        let input: String = read!();
        b.move_by_str(input.as_str());
    }*/
}
