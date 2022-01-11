#![allow(unused)]
use colored::Colorize;
use ndarray::prelude::*;

const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

//todo: square enum einführen. enthält piece oder empty
//piece enum einführen. Gibt allen Zahlen verständliche Namen und konvertiert sie
//mit index() oder so zur zahl. board[][] von i32 zu square ändern.
//anstatt direkt auf die zahl zuzugreifen, muss ich dann halt
//if let full(piece) = board[[y,x]] {piece.kind.index} benutzen (wenn ich die zahl
//überhaupt für etwas brauche. Ging ja eigentlich nur darum, wenig speicher zu brauchen
//und jetzt haben wir einfach ein kleines struct anstatt nur eine zahl).

struct Game {}

impl Game {
    fn new() -> Game {
        Game {}
    }
}

struct BoardState {
    board: Board,
}

///10x12 Array.
/// board[[2,1]] refers to the a8 square
pub struct Board {
    pub board: Array2<i32>, //vielleicht sollte ich einen array von piece enums machen
}

impl Board {
    pub fn new() -> Board {
        Board::from_fen(STARTING_POSITION)
    }

    ///Only field 1 works at the moment (only position layout, with no further info)
    pub fn from_fen(fen: &str) -> Board {
        let mut board = Array2::<i32>::zeros((12, 10));
        let mut rank = 8;
        let mut file = 1;

        //Field 1

        for c in fen.chars() {
            let (y, x) = Board::to_board(rank, file);
            if let Some(num) = c.to_digit(10) {
                file += num as usize;
                continue;
            }
            match c {
                '/' => {
                    rank -= 1;
                    file = 1;
                    continue;
                }
                'p' => board[[y, x]] = -1,
                'b' => board[[y, x]] = -2,
                'n' => board[[y, x]] = -3,
                'r' => board[[y, x]] = -4,
                'q' => board[[y, x]] = -5,
                'k' => board[[y, x]] = -6,
                'P' => board[[y, x]] = 1,
                'B' => board[[y, x]] = 2,
                'N' => board[[y, x]] = 3,
                'R' => board[[y, x]] = 4,
                'Q' => board[[y, x]] = 5,
                'K' => board[[y, x]] = 6,
                ' ' => break,
                _ => (),
            }
            file += 1;
        }
        Board { board }
    }

    ///Move piece in array without checking for legality.
    pub fn move_piece(
        &mut self,
        from_rank: usize,
        from_file: usize,
        to_rank: usize,
        to_file: usize,
    ) {
        let (yf, xf) = Board::to_board(from_rank, from_file);
        let (yt, xt) = Board::to_board(to_rank, to_file);
        let piece_val = self.board[[yf, xf]];
        self.board[[yf, xf]] = 0;
        self.board[[yt, xt]] = piece_val;
    }

    ///Move piece by string like "e2e4", without checking for legality.
    pub fn move_by_str(&mut self, arg: &str) {
        let mut chars = arg.chars();
        let from_file = Board::file_letter_to_number(chars.next().unwrap());
        let from_rank = chars.next().unwrap().to_digit(10).unwrap() as usize;
        let to_file = Board::file_letter_to_number(chars.next().unwrap());
        let to_rank = chars.next().unwrap().to_digit(10).unwrap() as usize;
        self.move_piece(from_rank, from_file, to_rank, to_file);
        self.draw();
    }

    ///Example: Converts a to 1.
    fn file_letter_to_number(rank: char) -> usize {
        match rank {
            'a' => 1,
            'b' => 2,
            'c' => 3,
            'd' => 4,
            'e' => 5,
            'f' => 6,
            'g' => 7,
            'h' => 8,
            _ => panic!(),
        }
    }

    /// Input are coordinates on the actual chess board.
    /// Output are coordinates on the 2d array with padding included.
    /// Output coordinate has y coordinate first (like ndarray)
    pub fn to_board(rank: usize, file: usize) -> (usize, usize) {
        (10 - rank, file)
    }

    ///Draw chess board using unicode characters. Colors seem inverted
    /// on black background
    pub fn draw(&self) {
        for rank in (1..9).rev() {
            for file in 1..9 {
                let (y, x) = Board::to_board(rank, file);
                let tile = match self.board[[y, x]].abs() {
                    0 => " ⠀",
                    1 => "♟︎ ",
                    2 => "♝ ",
                    3 => "♞ ",
                    4 => "♜ ",
                    5 => "♛ ",
                    6 => "♚ ",
                    _ => panic!(),
                };

                let tile = if (rank + file) % 2 == 0 {
                    tile.on_truecolor(158, 93, 30) //black
                } else {
                    tile.on_truecolor(205, 170, 125) //white
                };

                let tile = if self.board[[y, x]] > 0 {
                    tile.white()
                } else {
                    tile.black()
                };

                print!("{}", tile);
            }
            println!();
        }
        println!();
    }
}

struct Player1 {
    pieces: Vec<Piece>,
}

struct Player2 {}

enum Piece {
    Pawn(Pawn),
    Bishop(Bishop),
    Knight(Knight),
    Rook(Rook),
    Queen(Queen),
    King(King),
}

struct Pawn {
    size: i32,
}

struct Bishop {}

struct Knight {}

struct Rook {}

struct Queen {}

struct King {}

enum Color {
    Black,
    White,
}
