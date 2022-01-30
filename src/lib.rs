#![allow(unused)]
use colored::Colorize;
use ndarray::prelude::*;

const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

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

    ///Readonly!
    fn at(&self, rank: usize, file: usize) -> Square {
        let (y, x) = Board::to_board(rank, file);
        self.board[[y, x]]
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
                //better according to clippy
                if !(2..=9).contains(&i) {
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

        //This check is redundant:
        let square = match self.board[[yf, xf]] {
            Square::Full(p) => Square::Full(p),
            _ => panic!(
                "There is no piece on rank {}, file {}.",
                from_rank, from_file
            ),
        };
        if self
            .pseudo_legal_moves(from_rank, from_file, MoveType::Default)
            .contains(&(yt, xt))
        {
            eprintln!("allowed");
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

    ///When it's done, this should check for obstructions. Depends on the piece kind and
    /// the current position of the piece. Input: y and x on the array (with padding)
    /// Supports:
    /// -Pawn
    /// -Knight
    /// -King
    /// -Rook
    /// The following pieces already check for obstructions:
    /// -Pawn
    /// -Rook
    ///
    /// todo: MOVES IN EIGENES STRUCT/FILE TUN
    /// -attacking move für pawn ist anders als normaler move
    /// -struktur bei pawns richtig schlecht
    /// -sliding pieces brauchen viel zu lange für den check
    /// -sliding pieces müssten eigentlich nur eine richtung berechnen, wenn ich
    /// den vorgeschlagenen move, der überprüft wird, auch beachten würde
    ///
    /// -bei attack squares für pawn ganz anders, für alle pieces darf der erste, wo ein
    /// GEGENER draufsteht, auch angegriffen werden.
    pub fn pseudo_legal_moves(
        &self,
        rank: usize,
        file: usize,
        move_type: MoveType,
    ) -> Vec<(usize, usize)> {
        let (y, x) = Board::to_board(rank, file);
        match self.board[[y, x]] {
            Square::Full(p) => match p.kind {
                PieceKind::Pawn => self.pseudo_legal_pawn_moves(p.color, y, x, move_type),
                PieceKind::Knight => self.pseudo_legal_knight_moves(p.color, y, x, move_type),
                PieceKind::King => self.pseudo_legal_king_moves(y, x, move_type),
                PieceKind::Rook => self.pseudo_legal_rook_moves(y, x, move_type),
                _ => Vec::new(),
            },
            Square::Empty => {
                eprintln!(
                    "Rank {}, file {} is empty, but pseudo legal moves inquired.",
                    rank, file
                );
                Vec::new()
            }
            Square::Padding => {
                panic!("valid rank & file gave a padding square")
            }
        }
    }

    fn pseudo_legal_pawn_moves(
        &self,
        color: PieceColor,
        y: usize,
        x: usize,
        move_type: MoveType,
    ) -> Vec<(usize, usize)> {
        match color {
            PieceColor::Black => match move_type {
                MoveType::Default => match self.board[[y + 1, x]] {
                    Square::Empty => {
                        let mut v = vec![(y + 1, x)];
                        if y == 3 {
                            if let Square::Empty = self.board[[y + 2, x]] {
                                v.push((y + 2, x));
                            }
                        }
                        v
                    }
                    _ => Vec::new(),
                },
                MoveType::Attack => {
                    let mut v = Vec::new();
                    match self.board[[y + 1, x - 1]] {
                        //Left attack
                        Square::Full(p) if matches!(p.color, PieceColor::White) => {
                            v.push((y + 1, x - 1));
                        }
                        _ => (),
                    }
                    match self.board[[y + 1, x + 1]] {
                        //Right attack
                        Square::Full(p) if matches!(p.color, PieceColor::White) => {
                            v.push((y + 1, x + 1));
                        }
                        _ => (),
                    }
                    v
                }
            },
            PieceColor::White => match move_type {
                MoveType::Default => match self.board[[y - 1, x]] {
                    Square::Empty => {
                        let mut v = vec![(y - 1, x)];
                        if y == 8 {
                            if let Square::Empty = self.board[[y - 2, x]] {
                                v.push((y - 2, x));
                            }
                        }
                        v
                    }
                    _ => Vec::new(),
                },
                MoveType::Attack => {
                    let mut v = Vec::new();
                    match self.board[[y - 1, x - 1]] {
                        //Left attack
                        Square::Full(p) if matches!(p.color, PieceColor::Black) => {
                            v.push((y - 1, x - 1));
                        }
                        _ => (),
                    }
                    match self.board[[y - 1, x + 1]] {
                        //Right attack
                        Square::Full(p) if matches!(p.color, PieceColor::Black) => {
                            v.push((y - 1, x + 1));
                        }
                        _ => (),
                    }
                    v
                }
            },
        }
    }

    fn pseudo_legal_knight_moves(
        &self,
        color: PieceColor,
        y: usize,
        x: usize,
        move_type: MoveType,
    ) -> Vec<(usize, usize)> {
        let v = vec![
            (y - 2, x - 1),
            (y - 2, x + 1),
            (y - 1, x + 2),
            (y + 1, x + 2),
            (y + 2, x + 1),
            (y + 2, x - 1),
            (y + 1, x - 2),
            (y - 1, x - 2),
        ];

        //todo: tests für knight moves mit fressen/ohne!
        match move_type {
            MoveType::Default => v
                .into_iter()
                .filter(|&(y, x)| matches!(self.board[[y, x]], Square::Empty))
                .collect(),
            MoveType::Attack => {
                let opposite = color.opposite();
                v.into_iter()
                    .filter(|&(y, x)| {
                        matches!(self.board[[y, x]], Square::Empty)
                            || if let Square::Full(p) = self.board[[y, x]] {
                                matches!(p.color, opposite)
                            } else {
                                false
                            }
                    })
                    .collect()
            }
        }
    }

    fn pseudo_legal_king_moves(
        &self,
        y: usize,
        x: usize,
        move_type: MoveType,
    ) -> Vec<(usize, usize)> {
        vec![
            (y - 1, x - 1),
            (y - 1, x),
            (y - 1, x + 1),
            (y, x + 1),
            (y + 1, x + 1),
            (y + 1, x),
            (y + 1, x - 1),
            (y, x - 1),
        ]
    }

    fn pseudo_legal_rook_moves(
        &self,
        y: usize,
        x: usize,
        move_type: MoveType,
    ) -> Vec<(usize, usize)> {
        //Upwards
        let mut v = Vec::new();
        for i in (2..y).rev() {
            if let Square::Empty = self.board[[i, x]] {
                v.push((i, x));
            } else {
                break;
            }
        }
        //Downwards
        for i in y + 1..10 {
            if let Square::Empty = self.board[[i, x]] {
                v.push((i, x));
            } else {
                break;
            }
        }
        //Right
        for j in x + 1..9 {
            if let Square::Empty = self.board[[y, j]] {
                v.push((y, j));
            } else {
                break;
            }
        }
        //Left
        for j in (1..x).rev() {
            if let Square::Empty = self.board[[y, j]] {
                v.push((y, j));
            } else {
                break;
            }
        }
        v
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
    /// todo: could draw pseudolegal moves for a specific piece (maybe in
    /// another function)
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

impl Default for Board {
    fn default() -> Self {
        Self::new()
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

pub enum MoveType {
    Default,
    Attack,
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

    fn is_full(&self) -> bool {
        matches!(*self, Self::Full(p))
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

impl PieceColor {
    fn opposite(&self) -> Self {
        match *self {
            Self::Black => Self::White,
            Self::White => Self::Black,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    //Tests currently ignore whose turn it is and whether the position is even legal.

    #[test]
    fn white_pawn_take_left() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(4, 5, MoveType::Attack), vec![(5, 4)]);
    }

    #[test]
    fn white_pawn_not_take_center() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(4, 5, MoveType::Attack), vec![]);
    }

    #[test]
    fn white_pawn_take_right() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/5p2/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(4, 5, MoveType::Attack), vec![(5, 6)]);
    }

    #[test]
    fn black_pawn_take_left() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(5, 5, MoveType::Attack), vec![(6, 4)]);
    }

    #[test]
    fn black_pawn_not_take_center() {
        let b = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1");
    }

    #[test]
    fn black_pawn_take_right() {
        let b = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(5, 4, MoveType::Attack), vec![(6, 5)]);
    }

    #[test]
    fn white_pawn_double_push() {
        let b = Board::new();
        assert_eq!(
            b.pseudo_legal_moves(2, 1, MoveType::Default),
            vec![(7, 1), (6, 1)]
        );
    }

    #[test]
    fn black_pawn_double_push() {
        let b = Board::new();
        assert_eq!(
            b.pseudo_legal_moves(7, 1, MoveType::Default),
            vec![(4, 1), (5, 1)]
        );
    }

    #[test]
    fn white_pawn_double_push_blocked() {
        let b = Board::from_fen("rnbqkbnr/1ppppppp/8/8/p7/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(2, 1, MoveType::Default), vec![(7, 1)]);
    }

    #[test]
    fn black_pawn_double_push_blocked() {
        let b = Board::from_fen("rnbqkbnr/pppppppp/8/P7/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(7, 1, MoveType::Default), vec![(4, 1)]);
    }
}
