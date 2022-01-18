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
    pub board: Array2<Square>,
}

impl Board {
    pub fn new() -> Board {
        Board::from_fen(STARTING_POSITION)
    }

    ///Only field 1 works at the moment (only position layout, with no further info)
    pub fn from_fen(fen: &str) -> Board {
        let mut board = Array2::<Square>::default((12, 10));
        let mut rank = 8;
        let mut file = 1;

        //Field 1

        //Padding around the board
        for i in 0..12 {
            for j in 0..10 {
                if i < 2 || i > 9 {
                    board[[i, j]] = Square::Padding;
                } else {
                    board[[i, 0]] = Square::Padding;
                    board[[i, 9]] = Square::Padding;
                }
            }
        }

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
                'p' => board[[y, x]] = Square::Full(Piece::pawn(PieceColor::Black)),
                'b' => board[[y, x]] = Square::Full(Piece::bishop(PieceColor::Black)),
                'n' => board[[y, x]] = Square::Full(Piece::knight(PieceColor::Black)),
                'r' => board[[y, x]] = Square::Full(Piece::rook(PieceColor::Black)),
                'q' => board[[y, x]] = Square::Full(Piece::queen(PieceColor::Black)),
                'k' => board[[y, x]] = Square::Full(Piece::king(PieceColor::Black)),
                'P' => board[[y, x]] = Square::Full(Piece::pawn(PieceColor::White)),
                'B' => board[[y, x]] = Square::Full(Piece::bishop(PieceColor::White)),
                'N' => board[[y, x]] = Square::Full(Piece::knight(PieceColor::White)),
                'R' => board[[y, x]] = Square::Full(Piece::rook(PieceColor::White)),
                'Q' => board[[y, x]] = Square::Full(Piece::queen(PieceColor::White)),
                'K' => board[[y, x]] = Square::Full(Piece::king(PieceColor::White)),
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
        let square = match self.board[[yf, xf]] {
            Square::Full(p) => Square::Full(p),
            _ => panic!(
                "There is no piece on rank {}, file {}.",
                from_rank, from_file
            ),
        };
        if self.pseudo_legal_moves(yf, xf).unwrap().contains(&(yt, xt)) {
            println!("allowed");
        } else {
            eprintln!("not allowed");
        }
        self.board[[yf, xf]] = Square::Empty;
        self.board[[yt, xt]] = square;
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

    ///This SHOULD EVENTUALLY! check for obstructions. Only depends on the piece kind and
    /// the current position of the piece. Input: y and x on the array (with padding)
    pub fn pseudo_legal_moves(&self, y: usize, x: usize) -> Option<Vec<(usize, usize)>> {
        match self.board[[y, x]] {
            Square::Full(p) => Some(match p.kind {
                PieceKind::Pawn => match p.color {
                    PieceColor::Black => {
                        if y == 3 {
                            vec![(y + 1, x), (y + 2, x)]
                        } else {
                            vec![(y + 1, x)]
                        }
                    }
                    PieceColor::White => {
                        if y == 8 {
                            vec![(y - 1, x), (y - 2, x)]
                        } else {
                            vec![(y - 1, x)]
                        }
                    }
                },
                PieceKind::Knight => vec![
                    (y - 2, x - 1),
                    (y - 2, x + 1),
                    (y - 1, x + 2),
                    (y + 1, x + 2),
                    (y + 2, x + 1),
                    (y + 2, x - 1),
                    (y + 1, x - 2),
                    (y - 1, x - 2),
                ],
                PieceKind::King => vec![
                    (y - 1, x - 1),
                    (y - 1, x),
                    (y - 1, x + 1),
                    (y, x + 1),
                    (y + 1, x + 1),
                    (y + 1, x),
                    (y + 1, x - 1),
                    (y, x - 1),
                ],
                PieceKind::Rook => {
                    //Nach oben
                    Vec::new()
                }
                _ => Vec::new(),
            }),
            _ => None,
        }
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

    ///Draw chess board using unicode characters.
    pub fn draw(&self) {
        for rank in (1..9).rev() {
            for file in 1..9 {
                let (y, x) = Board::to_board(rank, file);
                let tile = match self.board[[y, x]].unicode_str() {
                    Some(s) => s,
                    None => continue,
                };

                let tile = if (rank + file) % 2 == 0 {
                    tile.on_truecolor(158, 93, 30) //black
                } else {
                    tile.on_truecolor(205, 170, 125) //white
                };

                let tile = match self.board[[y, x]].color() {
                    Some(color) => match color {
                        PieceColor::Black => tile.black(),
                        PieceColor::White => tile.white(),
                    },
                    None => tile,
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

#[derive(Clone, Copy, Debug)]
enum PieceKind {
    Pawn,
    Bishop,
    Knight,
    Rook,
    Queen,
    King,
}

#[derive(Clone, Copy, Debug)]
pub struct Piece {
    kind: PieceKind,
    color: PieceColor,
}

impl Piece {
    fn kind(&self) -> PieceKind {
        self.kind
    }

    fn color(&self) -> PieceColor {
        self.color
    }

    fn pawn(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Pawn,
            color,
        }
    }

    fn bishop(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Bishop,
            color,
        }
    }

    fn knight(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Knight,
            color,
        }
    }

    fn rook(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Rook,
            color,
        }
    }

    fn queen(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Queen,
            color,
        }
    }

    fn king(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::King,
            color,
        }
    }

    fn unicode_str(&self) -> &str {
        match self.kind {
            PieceKind::Pawn => "♟︎ ",
            PieceKind::Bishop => "♝ ",
            PieceKind::Knight => "♞ ",
            PieceKind::Rook => "♜ ",
            PieceKind::Queen => "♛ ",
            PieceKind::King => "♚ ",
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Square {
    Full(Piece),
    Empty,
    Padding,
}

impl Square {
    fn unicode_str(&self) -> Option<&str> {
        match self {
            Self::Full(p) => Some(p.unicode_str()),
            Self::Empty => Some(" ⠀"),
            Self::Padding => None,
        }
    }

    fn color(&self) -> Option<PieceColor> {
        match self {
            Self::Full(p) => Some(p.color),
            _ => None,
        }
    }
}

impl Default for Square {
    fn default() -> Self {
        Self::Empty
    }
}

#[derive(Clone, Copy, Debug)]
enum PieceColor {
    Black,
    White,
}
