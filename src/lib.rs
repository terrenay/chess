mod evaluation;

use std::fmt::{self, Display};

use colored::Colorize;
use ndarray::prelude::*;

const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
pub static mut COUNT: i32 = 0;

//Todo: should start to think about organizing this mess into multiple files

//Todo: three separate functions to draw is quite ugly. There must be a better way!

//Todo: user can enter "e2" and it shows possible moves from there

//Todo: -------->> evaluate current board state! <<------
/*1: piece tables für alle piece in berechnung miteinfliessen lassen (static)
2: negamax algorithmus!
3: castling erlauben
4: pawn promotion erlauben */
//5: doubled pawns, bishop pair miteinfliessen lassen
//Todo: kopien von board durch referenzen ersetzen

///Currently the idea is to have a single instance of BoardState that is then modified.
/// The other option would be to clone it each ply, but that seems like a waste of
/// resources.
///
/// For multithreading, I could later clone the BoardState at some node and have each
/// thread modify its own copy downwards.
pub struct BoardState {
    pub board: Board,
    turn: PieceColor,
    white_castling_rights: bool,
    black_castling_rights: bool,
    en_passant: Option<Field>,
    moves: Vec<Move>,
    taken: Vec<Piece>,
}

impl BoardState {
    pub fn new() -> Self {
        Self {
            board: Board::new(),
            turn: PieceColor::White,
            white_castling_rights: true,
            black_castling_rights: true,
            en_passant: None,
            moves: vec![],
            taken: vec![],
        }
    }

    pub fn evaluate(&self) -> i32 {
        unsafe {
            COUNT += 1;
        }
        evaluation::evaluate(self)
    }

    ///For every legal move in the current position, this function calls make,
    /// then calls itself recursively, and finally calls unmake.
    ///
    /// Depth is how many recursive calls it should do.
    pub fn min_max_helper(&mut self, depth: i32) -> i32 {
        if depth == 0 {
            self.evaluate()
        } else {
            let mut max = -i32::MAX;
            for m in self.generate_moves() {
                self.make(m); //Assume the move is legal. This must be ensured by genmoves.
                              //println!("AFTER MAKE {}", m);

                //self.draw();
                let score = -self.min_max_helper(depth - 1);
                self.unmake();
                if score > max {
                    max = score;
                }
            }
            //println!("Max was: {}", max);
            max
        }
    }

    ///Move piece by string like "e2e4". Checks for pseudo-legality.
    #[allow(clippy::needless_return)]
    pub fn move_by_str(&mut self, arg: &str) {
        println!("\nTry to process input: {}", arg);

        let mut chars = arg.chars();

        let from_file = Board::file_letter_to_number(chars.next().unwrap()) as i8;
        let from_rank = chars.next().unwrap().to_digit(10).unwrap() as i8;

        if let Square::Full(p) = self.board.at(from_rank, from_file) {
            if p.color != self.turn {
                eprintln!("{} to move!", self.turn.to_string().red());
                return;
            }
        } else {
            eprintln!(
                "{}",
                Error::NoPieceOnField(Field::new(from_rank, from_file))
                    .to_string()
                    .red()
            );
            return;
        }

        //From here on we can be sure that it's the correct side to move and the square is
        //actually full.

        let to_file = Board::file_letter_to_number(chars.next().unwrap()) as i8;
        let to_rank = chars.next().unwrap().to_digit(10).unwrap() as i8;

        let from = Field::new(from_rank, from_file);
        let to = Field::new(to_rank, to_file);

        let v = self
            .board
            .pseudo_legal_moves(from)
            .unwrap()
            .into_iter()
            .find(|m| m.from == from && m.to == to);

        if let Some(m) = v {
            self.make(Move::new(from, to, m.move_type))
        } else {
            eprintln!(
                "{}",
                Error::BadMoveTarget(Field::new(to_rank, to_file))
                    .to_string()
                    .red()
            );
        }
    }

    pub fn draw(&self) {
        self.board.draw();
        self.print_state_info();
    }

    fn print_state_info(&self) {
        println!("Turn: {:?}", self.turn);
        println!("White castling right: {:?}", self.white_castling_rights);
        println!("Black castling right: {:?}", self.black_castling_rights);
        println!("En passant square: {:?}", self.en_passant);
        println!("EVALUATION: {}", self.evaluate());
    }

    pub fn generate_moves(&self) -> Vec<Move> {
        //Going through all fields, checking whether there is a piece, then calling
        //pseudo_legal_moves feels quite inefficient...

        //Eventually I want to keep a list of currently alive pieces. Then only go through
        //those //todo!
        let mut v: Vec<Move> = vec![];
        for rank in 1..=8 {
            for file in 1..=8 {
                if let Square::Full(p) = self.board.at(rank, file) {
                    if p.color == self.turn {
                        v.extend(
                            self.board
                                .pseudo_legal_moves(Field::new(rank, file))
                                .unwrap(),
                        );
                    }
                }
            }
        }
        v
    }

    ///Assumes the move is legal!
    fn make(&mut self, m: Move) {
        //println!("make start {}", m);
        self.moves.push(m);
        if let Square::Full(moving_piece) = self.board.at(m.from.rank, m.from.file) {
            match m.move_type {
                MoveType::Default => (),

                MoveType::Capture => {
                    if let Square::Full(target) = self.board.at(m.to.rank, m.to.file) {
                        self.taken.push(target);
                    } else {
                        panic!("MoveType attack but empty target");
                    }
                }
                _ => todo!(),
            }

            self.board.set(m.from.rank, m.from.file, Square::Empty);
            self.board
                .set(m.to.rank, m.to.file, Square::Full(moving_piece));
        }
        self.turn = self.turn.opposite();
        //println!("make end {}", m);
        //Add the moves to self.moves, and if attacking move, add the taken piece to taken.
        //Add castle and promotion to movetype (they are all mutually exclusive).
        //If castle, restore or take away castling right for whose turn it is.
    }

    ///Unmakes the last move (the one at the end of the moves vec)
    fn unmake(&mut self) {
        //println!("unmake start");
        self.turn = self.turn.opposite();
        if let Some(m) = self.moves.pop() {
            //to and from are from the perspective of the original move.
            if let Square::Full(moving_piece) = self.board.at(m.to.rank, m.to.file) {
                self.board.set(m.to.rank, m.to.file, Square::Empty);
                self.board
                    .set(m.from.rank, m.from.file, Square::Full(moving_piece));
                match m.move_type {
                    MoveType::Default => (),

                    MoveType::Capture => {
                        if let Some(taken_piece) = self.taken.pop() {
                            self.board
                                .set(m.to.rank, m.to.file, Square::Full(taken_piece));
                        } else {
                            panic!("Trying to unmake attacking move but no piece taken");
                        }
                    }
                    _ => todo!(),
                }
            } else {
                self.draw();
                panic!("No piece at destination in unmake");
            }
        } else {
            panic!("Trying to unmake but there is no previous move");
        }
        //println!("unmake end");
        //Revert the above by popping from the stack
    }
}

impl Default for BoardState {
    fn default() -> Self {
        Self::new()
    }
}

///Every piece on the board corresponds to one instance of UniquePiece.
/// The instances stay the same over several moves, only their positions change.
/// This is used in the make() and unmake() functions of BoardState.
struct UniquePiece {
    piece: Piece,
    field: Field,
}

///If move_type==Attack, muss target auf stack gepusht/gepoppt werden!
#[derive(PartialEq, Copy, Clone)]
pub struct Move {
    from: Field,
    to: Field,
    move_type: MoveType,
    //Todo! Movetype entfernen? das weiss der move selbst noch nicht, muss immer berechnet
    //werden. aber wie castle speichern? Für movegen vielleicht capture only mode behalten,
    //ansonsten brauch ich die eigentlich für nichts.

    //nur allmoves und captures.
}

impl Move {
    fn new(from: Field, to: Field, move_type: MoveType) -> Self {
        Move {
            from,
            to,
            move_type,
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.from, self.to)
    }
}

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
    ///
    /// <b>Precondition: -1 <= rank, file <= 10
    fn at(&self, rank: i8, file: i8) -> Square {
        let (i, j) = Board::to_board(rank, file);
        self.board[[i, j]]
    }

    /// <b>Precondition: -1 <= rank, file <= 10
    fn set(&mut self, rank: i8, file: i8, val: Square) {
        let (i, j) = Board::to_board(rank, file);
        self.board[[i, j]] = val;
    }

    ///Only field 1 works at the moment (only position layout, with no further info)
    fn from_fen(fen: &str) -> Board {
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

    ///When it's done, this should check for obstructions. Depends on the piece kind and
    /// the current position of the piece. Input: y and x on the array (with padding)
    /// Supports:
    /// -Pawn
    /// -Knight
    /// -King
    /// -Rook
    ///
    /// todo: MOVES IN EIGENES STRUCT/FILE TUN
    /// -sliding pieces brauchen viel zu lange für den check
    /// -sliding pieces müssten eigentlich nur eine richtung berechnen, wenn ich
    /// den vorgeschlagenen move, der überprüft wird, auch beachten würde
    fn pseudo_legal_moves(&self, from: Field) -> Result<Vec<Move>, Error> {
        let Field { rank, file } = from;
        match self.at(rank, file) {
            Square::Full(p) => match p.kind {
                PieceKind::Pawn => Ok(self.pseudo_legal_pawn_moves(p.color, from)),
                PieceKind::Knight => Ok(self.pseudo_legal_knight_moves(p.color, from)),
                PieceKind::King => Ok(self.pseudo_legal_king_moves(p.color, from)),
                PieceKind::Rook => Ok(self.pseudo_legal_rook_moves(p.color, from)),
                PieceKind::Bishop => Ok(self.pseudo_legal_bishop_moves(p.color, from)),
                PieceKind::Queen => Ok(self.pseudo_legal_queen_moves(p.color, from)),
            },
            _ => Err(Error::NoPieceOnField(Field::new(rank, file))),
        }
    }

    fn pseudo_legal_pawn_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let Field { rank, file } = from;
        match color {
            PieceColor::Black => {
                let mut v = match self.at(rank - 1, file) {
                    Square::Empty => {
                        let mut v = vec![Move::new(
                            from,
                            Field::new(rank - 1, file),
                            MoveType::Default,
                        )];

                        if rank == 7 {
                            //Double push
                            if let Square::Empty = self.at(rank - 2, file) {
                                v.push(Move::new(
                                    from,
                                    Field::new(rank - 2, file),
                                    MoveType::Default,
                                ));
                            }
                        }
                        v
                    }
                    _ => vec![],
                };
                //Black pawn attacks down to the left
                match self.at(rank - 1, file - 1) {
                    Square::Full(p) if matches!(p.color, PieceColor::White) => {
                        v.push(Move::new(
                            from,
                            Field::new(rank - 1, file - 1),
                            MoveType::Capture,
                        ));
                    }
                    _ => (),
                }

                //Black pawn attacks down to the right
                match self.at(rank - 1, file + 1) {
                    //Right attack
                    Square::Full(p) if matches!(p.color, PieceColor::White) => {
                        v.push(Move::new(
                            from,
                            Field::new(rank - 1, file + 1),
                            MoveType::Capture,
                        ));
                    }
                    _ => (),
                }
                v
            }
            PieceColor::White => {
                let mut v = match self.at(rank + 1, file) {
                    Square::Empty => {
                        let mut v = vec![Move::new(
                            from,
                            Field::new(rank + 1, file),
                            MoveType::Default,
                        )];

                        if rank == 2 {
                            //Double push
                            if let Square::Empty = self.at(rank + 2, file) {
                                v.push(Move::new(
                                    from,
                                    Field::new(rank + 2, file),
                                    MoveType::Default,
                                ));
                            }
                        }
                        v
                    }
                    _ => vec![],
                };

                //White pawn attacks up to the left
                match self.at(rank + 1, file - 1) {
                    Square::Full(p) if matches!(p.color, PieceColor::Black) => {
                        v.push(Move::new(
                            from,
                            Field::new(rank + 1, file - 1),
                            MoveType::Capture,
                        ));
                    }
                    _ => (),
                }

                //White pawn attacks up to the right
                match self.at(rank + 1, file + 1) {
                    Square::Full(p) if matches!(p.color, PieceColor::Black) => {
                        v.push(Move::new(
                            from,
                            Field::new(rank + 1, file + 1),
                            MoveType::Capture,
                        ));
                    }
                    _ => (),
                }
                v
            }
        }
    }

    fn pseudo_legal_knight_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let Field { rank, file } = from;
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
        self.filter_free_or_opponent(from, v, color.opposite())
    }

    ///This is quite scuffed at the moment since king moves are quite useless
    /// without checking for checks...
    /// todo: enemy checks
    fn pseudo_legal_king_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let Field { rank, file } = from;
        let v = vec![
            Field::new(rank + 1, file - 1),
            Field::new(rank + 1, file),
            Field::new(rank + 1, file + 2),
            Field::new(rank, file + 1),
            Field::new(rank - 1, file + 1),
            Field::new(rank - 1, file),
            Field::new(rank - 1, file - 1),
            Field::new(rank, file - 1),
        ];
        self.filter_free_or_opponent(from, v, color.opposite())
    }

    fn pseudo_legal_rook_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let Field { rank, file } = from;
        //Upwards
        let mut v = vec![];
        for i in rank + 1..=8 {
            let to = Field::new(i, file);
            if self.insert_or_break_loop(color, from, to, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Downwards
        for i in (1..=rank - 1).rev() {
            let to = Field::new(i, file);
            if self.insert_or_break_loop(color, from, to, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Right
        for j in file + 1..=8 {
            let to = Field::new(rank, j);
            if self.insert_or_break_loop(color, from, to, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Left
        for j in (1..=file - 1).rev() {
            let to = Field::new(rank, j);
            if self.insert_or_break_loop(color, from, to, &mut v) == BreakLoop::True {
                break;
            }
        }
        v
    }

    fn pseudo_legal_bishop_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let mut v = vec![];
        let mut diff = 1;
        let Field { rank, file } = from;

        //Todo: bekommt hier irgendwie eine alte version
        //des boards. Vielleicht muss ich das mehr mit
        //Referenzen rumpassen?

        //Up right
        while rank + diff <= 8 && file + diff <= 8 {
            let to = Field::new(rank + diff, file + diff);
            if self.insert_or_break_loop(color, from, to, &mut v) == BreakLoop::True {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Up left
        while rank + diff <= 8 && file - diff >= 1 {
            let to = Field::new(rank + diff, file - diff);
            if self.insert_or_break_loop(color, from, to, &mut v) == BreakLoop::True {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Down right
        while rank - diff >= 1 && file + diff <= 8 {
            let to = Field::new(rank - diff, file + diff);
            if self.insert_or_break_loop(color, from, to, &mut v) == BreakLoop::True {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Down left
        while rank - diff >= 1 && file - diff >= 1 {
            let to = Field::new(rank - diff, file - diff);
            if self.insert_or_break_loop(color, from, to, &mut v) == BreakLoop::True {
                break;
            }
            diff += 1;
        }
        v
    }

    ///Inefficiency at its peak
    fn pseudo_legal_queen_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let mut v = self.pseudo_legal_rook_moves(color, from);
        v.extend(self.pseudo_legal_bishop_moves(color, from));
        v
    }
    ///This function is intended to be used in a loop for a sliding piece which follows that
    /// piece's move direction and checks each square for obstruction by an opponent or own piece.
    /// This function returns BreakLoop::True if the current square is full or padding.
    /// It returns BreakLoop::False if the square is empty.
    /// It pushes coordinates into vector v if the color is the opponent's color or if it's empty.
    fn insert_or_break_loop(
        &self,
        color: PieceColor,
        from: Field,
        to: Field,
        v: &mut Vec<Move>,
    ) -> BreakLoop {
        let Field { rank, file } = to;
        match self.at(rank, file) {
            Square::Empty => {
                v.push(Move::new(from, to, MoveType::Default));
            }
            Square::Full(p) => {
                if p.color == color.opposite() {
                    v.push(Move::new(from, to, MoveType::Capture));
                }

                return BreakLoop::True;
            }
            Square::Padding => {
                panic!("invalid argument for insert_or_break_loop")
            }
        }
        BreakLoop::False
    }

    ///Given a list of possible target fields, remove those that would land on a piece of
    ///the same color as the moving piece. For the others, create a move of type capture
    /// or default.
    fn filter_free_or_opponent(
        &self,
        from: Field,
        v: Vec<Field>,
        opposite: PieceColor,
    ) -> Vec<Move> {
        let mut default_moves: Vec<Move> = v
            .iter()
            .filter(|to| matches!(self.at(to.rank, to.file), Square::Empty))
            .map(|&to| Move::new(from, to, MoveType::Default))
            .collect();

        let captures: Vec<Move> = v
            .iter()
            .filter(|to| {
                if let Square::Full(p) = self.at(to.rank, to.file) {
                    p.color == opposite
                } else {
                    false
                }
            })
            .map(|&to| Move::new(from, to, MoveType::Capture))
            .collect();

        default_moves.extend(captures);
        default_moves
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
    ///
    /// <b>Precondition: -1 <= rank, file <= 10
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
            print!("{}", rank);
            for file in 1..9 {
                let square = self.at(rank, file);
                let tile = match square.unicode_str() {
                    Some(s) => s,
                    None => continue,
                };

                let tile = if (rank + file) % 2 == 0 {
                    tile.on_truecolor(158, 93, 30) //black
                } else {
                    tile.on_truecolor(155, 120, 70) //white
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
        for i in 1..=8 {
            print!(" {}", Board::number_to_file_letter(i));
        }
        println!();
    }
    /*
    pub fn draw_pseudo_legal_moves(&self, pos_rank: i8, pos_file: i8) {
        let v = self.pseudo_legal_moves(pos_rank, pos_file).unwrap();
        for rank in (1..9).rev() {
            print!("{}", rank);
            for file in 1..9 {
                let square = self.at(rank, file);
                let tile = match square.unicode_str() {
                    Some(s) => s,
                    None => continue,
                };

                let tile = if (rank + file) % 2 == 0 {
                    tile.on_truecolor(158, 93, 30) //black
                } else {
                    tile.on_truecolor(155, 120, 70) //white
                };

                let tile = if v.contains(&Move::new(
                    Field::new(pos_rank, pos_file),
                    Field::new(rank, file),
                )) {
                    tile.on_truecolor(255, 0, 0)
                } else {
                    tile
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
        for i in 1..=8 {
            print!(" {}", Board::number_to_file_letter(i));
        }
        println!();
    }*/
    /*
    pub fn draw_pseudo_legal_moves_and_prev_move(
        &self,
        from_rank: i8,
        from_file: i8,
        to_rank: i8,
        to_file: i8,
        move_type: MoveType,
    ) {
        let v = self
            .pseudo_legal_moves(to_rank, to_file, move_type)
            .unwrap();
        for rank in (1..9).rev() {
            print!("{}", rank);
            for file in 1..9 {
                let square = self.at(rank, file);
                let tile = match square.unicode_str() {
                    Some(s) => s,
                    None => continue,
                };

                let tile = if (rank + file) % 2 == 0 {
                    tile.on_truecolor(158, 93, 30) //black
                } else {
                    tile.on_truecolor(155, 120, 70) //white
                };

                let tile = if (rank == from_rank && file == from_file)
                    || (rank == to_rank && file == to_file)
                {
                    tile.on_truecolor(0, 0, 255)
                } else {
                    tile
                };

                let tile = if v.contains(&Move::new(
                    Field::new(from_rank, from_file),
                    Field::new(rank, file),
                    MoveType::DefaultOrAttack,
                )) {
                    if rank == from_rank && file == from_file {
                        tile.on_truecolor(150, 0, 150)
                    } else {
                        tile.on_truecolor(255, 0, 0)
                    }
                } else {
                    tile
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
        for i in 1..=8 {
            print!(" {}", Board::number_to_file_letter(i));
        }
        println!();
    }*/
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

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

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum MoveType {
    Default,
    Capture,
    Castle,
    Promotion,
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

#[derive(PartialEq)]
enum BreakLoop {
    True,
    False,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
pub struct Field {
    rank: i8,
    file: i8,
}

impl Field {
    fn new(rank: i8, file: i8) -> Self {
        Self { rank, file }
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            Board::number_to_file_letter(self.file as usize),
            self.rank
        )
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

impl Display for PieceColor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            PieceColor::Black => write!(f, "Black"),
            PieceColor::White => write!(f, "White"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("square on {0} is empty or padding")]
    NoPieceOnField(Field),
    #[error("cannot move to {0}")]
    BadMoveTarget(Field),
}
/*
#[cfg(test)]
mod tests {
    use super::*;

    //Tests currently ignore whose turn it is and whether the position is even legal.

    #[test]
    fn white_pawn_take_left() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(4, 5, MoveType::Attack).unwrap(),
            vec![Field::new(5, 4)],
        );
    }

    #[test]
    fn white_pawn_not_take_center() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(4, 5, MoveType::Attack).unwrap(),
            vec![],
        );
    }

    #[test]
    fn white_pawn_take_right() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/5p2/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(4, 5, MoveType::Attack).unwrap(),
            vec![Field::new(5, 6)],
        );
    }

    #[test]
    fn black_pawn_take_left() {
        let b = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(5, 5, MoveType::Attack).unwrap(),
            vec![Field::new(4, 4)],
        );
    }

    #[test]
    fn black_pawn_not_take_center() {
        let b = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(5, 4, MoveType::Attack).unwrap(),
            vec![],
        );
    }

    #[test]
    fn black_pawn_take_right() {
        let b = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(5, 4, MoveType::Attack).unwrap(),
            vec![Field::new(4, 5)],
        );
    }

    #[test]
    fn white_pawn_double_push() {
        let b = Board::new();
        eq_fields(
            b.pseudo_legal_moves(2, 1, MoveType::Default).unwrap(),
            vec![Field::new(3, 1), Field::new(4, 1)],
        );
    }

    #[test]
    fn black_pawn_double_push() {
        let b = Board::new();
        eq_fields(
            b.pseudo_legal_moves(7, 1, MoveType::Default).unwrap(),
            vec![Field::new(6, 1), Field::new(5, 1)],
        );
    }

    #[test]
    fn white_pawn_double_push_blocked() {
        let b = Board::from_fen("rnbqkbnr/1ppppppp/8/8/p7/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(2, 1, MoveType::Default).unwrap(),
            vec![Field::new(3, 1)],
        );
    }

    #[test]
    fn black_pawn_double_push_blocked() {
        let b = Board::from_fen("rnbqkbnr/pppppppp/8/P7/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(7, 1, MoveType::Default).unwrap(),
            vec![Field::new(6, 1)],
        );
    }

    #[test]
    fn white_pawn_not_take_white_pawn() {
        let b = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(2, 4, MoveType::Attack).unwrap(),
            vec![],
        );
    }

    #[test]
    fn white_knight_takes() {
        let b = Board::from_fen("rnbqkbnr/pp1ppppp/8/8/8/2p5/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        eq_fields(
            b.pseudo_legal_moves(1, 2, MoveType::Attack).unwrap(),
            vec![Field::new(3, 1), Field::new(3, 3)],
        );
    }

    #[test]
    fn black_knight_corner_blocked() {
        let b = Board::from_fen("n7/2p5/1p6/8/8/8/8/8 w - - 0 1");
        eq_fields(
            b.pseudo_legal_moves(8, 1, MoveType::Attack).unwrap(),
            vec![],
        );
    }

    #[test]
    fn black_knight_corner_takes() {
        let b = Board::from_fen("n7/2p5/1P6/8/8/8/8/8 w - - 0 1");
        eq_fields(
            b.pseudo_legal_moves(8, 1, MoveType::Attack).unwrap(),
            vec![Field::new(6, 2)],
        );
    }

    #[test]
    fn black_knight_edge_takes() {
        let b = Board::from_fen("8/5p2/7n/8/6P1/8/8/8 w - - 0 1");
        eq_fields(
            b.pseudo_legal_moves(6, 8, MoveType::Attack).unwrap(),
            vec![Field::new(8, 7), Field::new(5, 6), Field::new(4, 7)],
        );
    }

    #[test]
    fn black_knight_edge_blocked() {
        let b = Board::from_fen("6p1/5p2/7n/5p2/6p1/8/8/8 w - - 0 1");
        eq_fields(
            b.pseudo_legal_moves(6, 8, MoveType::Attack).unwrap(),
            vec![],
        );
    }

    #[test]
    fn white_bishop_default_partially_blocked() {
        let b = Board::from_fen("8/8/8/4R3/1p6/2B5/8/p7 w - - 0 1");
        eq_fields(
            b.pseudo_legal_moves(3, 3, MoveType::Default).unwrap(),
            vec![
                Field::new(2, 2),
                Field::new(4, 4),
                Field::new(2, 4),
                Field::new(1, 5),
            ],
        );
    }

    #[test]
    fn white_bishop_attack_partially_blocked() {
        let b = Board::from_fen("8/8/8/4R3/1p6/2B5/8/p7 w - - 0 1");
        eq_fields(
            b.pseudo_legal_moves(3, 3, MoveType::Attack).unwrap(),
            vec![
                Field::new(2, 2),
                Field::new(4, 4),
                Field::new(2, 4),
                Field::new(1, 5),
                Field::new(4, 2),
                Field::new(1, 1),
            ],
        );
    }

    ///Compare v1 and v2, ignoring the order of the fields
    fn eq_fields(mut v1: Vec<Field>, mut v2: Vec<Field>) {
        v1.sort_unstable();
        v2.sort_unstable();
        assert_eq!(v1, v2);
    }
}
*/
