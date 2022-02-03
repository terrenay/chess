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
    board: Array2<Square>,
}

impl Board {
    pub fn new() -> Board {
        Board::from_fen(STARTING_POSITION)
    }

    ///Readonly!
    /// Out-of-bounds are supported as far as padding goes. That is, rank 0 and rank -1
    /// would be padding rows, but rank -2 would panic.
    fn at(&self, rank: i8, file: i8) -> Square {
        let (i, j) = Board::to_board(rank, file);
        self.board[[i, j]]
    }

    fn set(&mut self, rank: i8, file: i8, val: Square) {
        let (i, j) = Board::to_board(rank, file);
        self.board[[i, j]] = val;
    }

    ///Only field 1 works at the moment (only position layout, with no further info)
    pub fn from_fen(fen: &str) -> Board {
        let mut board = Array2::<Square>::default((12, 12));
        let mut rank = 8;
        let mut file = 1;

        //Field 1

        //Padding around the board
        for i in 0..12 {
            for j in 0..12 {
                //better according to clippy
                if !(2..=9).contains(&i) {
                    board[[i, j]] = Square::Padding;
                } else {
                    board[[i, 0]] = Square::Padding;
                    board[[i, 1]] = Square::Padding;
                    board[[i, 10]] = Square::Padding;
                    board[[i, 11]] = Square::Padding;
                }
            }
        }

        for c in fen.chars() {
            let (i, j) = Board::to_board(rank, file);
            if let Some(num) = c.to_digit(10) {
                file += num as i8;
                continue;
            }
            match c {
                '/' => {
                    rank -= 1;
                    file = 1;
                    continue;
                }
                'p' => board[[i, j]] = Square::Full(Piece::pawn(PieceColor::Black)),
                'b' => board[[i, j]] = Square::Full(Piece::bishop(PieceColor::Black)),
                'n' => board[[i, j]] = Square::Full(Piece::knight(PieceColor::Black)),
                'r' => board[[i, j]] = Square::Full(Piece::rook(PieceColor::Black)),
                'q' => board[[i, j]] = Square::Full(Piece::queen(PieceColor::Black)),
                'k' => board[[i, j]] = Square::Full(Piece::king(PieceColor::Black)),
                'P' => board[[i, j]] = Square::Full(Piece::pawn(PieceColor::White)),
                'B' => board[[i, j]] = Square::Full(Piece::bishop(PieceColor::White)),
                'N' => board[[i, j]] = Square::Full(Piece::knight(PieceColor::White)),
                'R' => board[[i, j]] = Square::Full(Piece::rook(PieceColor::White)),
                'Q' => board[[i, j]] = Square::Full(Piece::queen(PieceColor::White)),
                'K' => board[[i, j]] = Square::Full(Piece::king(PieceColor::White)),
                ' ' => break,
                _ => (),
            }
            file += 1;
        }
        Board { board }
    }

    ///Check for piece move legality (attack or default) & obstruction.
    /// Does not yet check for checks or pins.
    pub fn move_piece(&mut self, from_rank: i8, from_file: i8, to_rank: i8, to_file: i8) {
        //This check is basically redundant:
        let square = match self.at(from_rank, from_file) {
            Square::Full(p) => Square::Full(p),
            _ => panic!(
                "There is no piece on rank {}, file {}.",
                from_rank, from_file
            ),
        };

        if self
            .pseudo_legal_moves(from_rank, from_file, MoveType::Default)
            .contains(&Field::new(to_rank, to_file))
            || self
                .pseudo_legal_moves(from_rank, from_file, MoveType::Attack)
                .contains(&Field::new(to_rank, to_file))
        {
            eprintln!("allowed");
        } else {
            eprintln!("not allowed");
        }
        self.set(from_rank, from_file, Square::Empty);
        self.set(to_rank, to_file, square);
    }

    ///Move piece by string like "e2e4", without checking for legality.
    pub fn move_by_str(&mut self, arg: &str) {
        let mut chars = arg.chars();
        let from_file = Board::file_letter_to_number(chars.next().unwrap());
        let from_rank = chars.next().unwrap().to_digit(10).unwrap();
        let to_file = Board::file_letter_to_number(chars.next().unwrap());
        let to_rank = chars.next().unwrap().to_digit(10).unwrap();
        self.move_piece(
            from_rank as i8,
            from_file as i8,
            to_rank as i8,
            to_file as i8,
        );
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
    pub fn pseudo_legal_moves(&self, rank: i8, file: i8, move_type: MoveType) -> Vec<Field> {
        match self.at(rank, file) {
            Square::Full(p) => match p.kind {
                PieceKind::Pawn => self.pseudo_legal_pawn_moves(p.color, rank, file, move_type),
                PieceKind::Knight => self.pseudo_legal_knight_moves(p.color, rank, file, move_type),
                PieceKind::King => self.pseudo_legal_king_moves(rank, file, move_type),
                PieceKind::Rook => self.pseudo_legal_rook_moves(rank, file, move_type),
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
        rank: i8,
        file: i8,
        move_type: MoveType,
    ) -> Vec<Field> {
        match color {
            PieceColor::Black => match move_type {
                MoveType::Default => match self.at(rank - 1, file) {
                    Square::Empty => {
                        let mut v = vec![Field::new(rank - 1, file)];

                        if rank == 7 {
                            //Double push
                            if let Square::Empty = self.at(rank - 2, file) {
                                v.push(Field::new(rank - 2, file));
                            }
                        }
                        v
                    }
                    _ => vec![],
                },
                MoveType::Attack => {
                    let mut v = vec![];

                    //Black pawn attacks down to the left
                    match self.at(rank - 1, file - 1) {
                        Square::Full(p) if matches!(p.color, PieceColor::White) => {
                            v.push(Field::new(rank - 1, file - 1));
                        }
                        _ => (),
                    }

                    //Black pawn attacks down to the right
                    match self.at(rank - 1, file + 1) {
                        //Right attack
                        Square::Full(p) if matches!(p.color, PieceColor::White) => {
                            v.push(Field::new(rank - 1, file + 1));
                        }
                        _ => (),
                    }
                    v
                }
            },
            PieceColor::White => match move_type {
                MoveType::Default => match self.at(rank + 1, file) {
                    Square::Empty => {
                        let mut v = vec![Field::new(rank + 1, file)];

                        if rank == 2 {
                            //Double push
                            if let Square::Empty = self.at(rank + 2, file) {
                                v.push(Field::new(rank + 2, file));
                            }
                        }
                        v
                    }
                    _ => vec![],
                },
                MoveType::Attack => {
                    let mut v = Vec::new();

                    //White pawn attacks up to the left
                    match self.at(rank + 1, file - 1) {
                        Square::Full(p) if matches!(p.color, PieceColor::Black) => {
                            v.push(Field::new(rank + 1, file - 1));
                        }
                        _ => (),
                    }

                    //White pawn attacks up to the right
                    match self.at(rank + 1, file + 1) {
                        Square::Full(p) if matches!(p.color, PieceColor::Black) => {
                            v.push(Field::new(rank + 1, file + 1));
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
        rank: i8,
        file: i8,
        move_type: MoveType,
    ) -> Vec<Field> {
        let v = vec![
            Field::new(rank + 2, file - 1),
            Field::new(rank + 2, file + 1),
            Field::new(rank + 1, file + 2),
            Field::new(rank - 1, file + 2),
            Field::new(rank - 2, file + 1),
            Field::new(rank - 2, file - 1),
            Field::new(rank - 1, file - 2),
            Field::new(rank + 1, file - 2),
        ];

        match move_type {
            MoveType::Default => v
                .into_iter()
                .filter(|field| matches!(self.at(field.rank, field.file), Square::Empty))
                .collect(),
            MoveType::Attack => {
                let opposite = color.opposite();
                v.into_iter()
                    .filter(|field| {
                        matches!(self.at(field.rank, field.file), Square::Empty)
                            || if let Square::Full(p) = self.at(field.rank, field.file) {
                                //matches! war hier falsch. Da drin darf ich nur enum
                                //variants vergleichen. Hier will ich die Variante einer
                                //Variable benutzen.
                                p.color == opposite
                            } else {
                                false
                            }
                    })
                    .collect()
                //todo: das filtern nach piece oder empty in separate funktion
            }
        }
    }

    fn pseudo_legal_king_moves(&self, rank: i8, file: i8, move_type: MoveType) -> Vec<Field> {
        vec![
            Field::new(rank + 1, file - 1),
            Field::new(rank + 1, file),
            Field::new(rank + 1, file + 2),
            Field::new(rank, file + 1),
            Field::new(rank - 1, file + 1),
            Field::new(rank - 1, file),
            Field::new(rank - 1, file - 1),
            Field::new(rank, file - 1),
        ]
    }

    fn pseudo_legal_rook_moves(&self, rank: i8, file: i8, move_type: MoveType) -> Vec<Field> {
        //Upwards
        let mut v = Vec::new();
        for i in rank + 1..=8 {
            if let Square::Empty = self.at(i, file) {
                v.push(Field::new(i, file));
            } else {
                break;
            }
        }
        //Downwards
        for i in (1..=rank - 1).rev() {
            if let Square::Empty = self.at(i, file) {
                v.push(Field::new(i, file));
            } else {
                break;
            }
        }
        //Right
        for j in file + 1..=8 {
            if let Square::Empty = self.at(rank, j) {
                v.push(Field::new(rank, j));
            } else {
                break;
            }
        }
        //Left
        for j in (1..=file - 1).rev() {
            if let Square::Empty = self.at(rank, j) {
                v.push(Field::new(rank, j));
            } else {
                break;
            }
        }
        v
    }

    ///Example: Converts a to 1.
    fn file_letter_to_number(file: char) -> usize {
        match file {
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

    fn number_to_file_letter(file: usize) -> char {
        match file {
            1 => 'a',
            2 => 'b',
            3 => 'c',
            4 => 'd',
            5 => 'e',
            6 => 'f',
            7 => 'g',
            8 => 'h',
            _ => panic!(),
        }
    }

    /// Input are coordinates on the actual chess board.
    /// Output are coordinates on the 2d array with padding included.
    /// Output coordinate has y coordinate first (like ndarray)
    pub fn to_board(rank: i8, file: i8) -> (usize, usize) {
        //Rank is never bigger than 10, so 10-rank is guaranteed to be >= 0.
        //Rank 10 is needed in the knight's move generation if the knight is on
        //rank 8. Analogously, rank -1 is needed if it is on rank 1.
        //Thus we accept unsigned ints.
        debug_assert!(rank >= -1 && rank <= 10 && file >= -1 && file <= 10);
        ((10 - rank) as usize, (file + 1) as usize)
    }

    ///Draw chess board using unicode characters.
    /// todo: could draw pseudolegal moves for a specific piece (maybe in
    /// another function)
    pub fn draw(&self) {
        for rank in (1..9).rev() {
            for file in 1..9 {
                let square = self.at(rank, file);
                let tile = match square.unicode_str() {
                    Some(s) => s,
                    None => continue,
                };

                let tile = if (rank + file) % 2 == 0 {
                    tile.on_truecolor(158, 93, 30) //black
                } else {
                    tile.on_truecolor(205, 170, 125) //white
                };

                let tile = match square.color() {
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
}

impl Default for Square {
    fn default() -> Self {
        Self::Empty
    }
}

#[derive(PartialEq, Debug)]
pub struct Field {
    rank: i8,
    file: i8,
}

impl Field {
    fn new(rank: i8, file: i8) -> Self {
        Self { rank, file }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
        assert_eq!(
            b.pseudo_legal_moves(4, 5, MoveType::Attack),
            vec![Field::new(5, 4)]
        );
    }

    #[test]
    fn white_pawn_not_take_center() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(4, 5, MoveType::Attack), vec![]);
    }

    #[test]
    fn white_pawn_take_right() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/5p2/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(
            b.pseudo_legal_moves(4, 5, MoveType::Attack),
            vec![Field::new(5, 6)]
        );
    }

    #[test]
    fn black_pawn_take_left() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(
            b.pseudo_legal_moves(5, 5, MoveType::Attack),
            vec![Field::new(4, 4)]
        );
    }

    #[test]
    fn black_pawn_not_take_center() {
        let b = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(5, 4, MoveType::Attack), vec![]);
    }

    #[test]
    fn black_pawn_take_right() {
        let b = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(
            b.pseudo_legal_moves(5, 4, MoveType::Attack),
            vec![Field::new(4, 5)]
        );
    }

    #[test]
    fn white_pawn_double_push() {
        let b = Board::new();
        assert_eq!(
            b.pseudo_legal_moves(2, 1, MoveType::Default),
            vec![Field::new(3, 1), Field::new(4, 1)]
        );
    }

    #[test]
    fn black_pawn_double_push() {
        let b = Board::new();
        assert_eq!(
            b.pseudo_legal_moves(7, 1, MoveType::Default),
            vec![Field::new(6, 1), Field::new(5, 1)]
        );
    }

    #[test]
    fn white_pawn_double_push_blocked() {
        let b = Board::from_fen("rnbqkbnr/1ppppppp/8/8/p7/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(
            b.pseudo_legal_moves(2, 1, MoveType::Default),
            vec![Field::new(3, 1)]
        );
    }

    #[test]
    fn black_pawn_double_push_blocked() {
        let b = Board::from_fen("rnbqkbnr/pppppppp/8/P7/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(
            b.pseudo_legal_moves(7, 1, MoveType::Default),
            vec![Field::new(6, 1)]
        );
    }

    #[test]
    fn white_pawn_not_take_white_pawn() {
        let b = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(b.pseudo_legal_moves(2, 4, MoveType::Attack), vec![]);
    }

    #[test]
    fn white_knight_takes() {
        let b = Board::from_fen("rnbqkbnr/pp1ppppp/8/8/8/2p5/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(
            b.pseudo_legal_moves(1, 2, MoveType::Attack),
            vec![Field::new(3, 1), Field::new(3, 3)]
        );
    }
}
