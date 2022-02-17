mod evaluation;

use std::fmt::{self, Display};

use colored::Colorize;
use ndarray::prelude::*;

const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

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
    taken: Vec<UniquePiece>,
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
        evaluation::evaluate(self)
    }

    ///For every legal move in the current position, this function calls make,
    /// then calls itself recursively, and finally calls unmake.
    ///
    /// Depth is how many recursive calls it should do.
    fn min_max_helper(&self, depth: i32) -> i32 {
        if depth == 0 {
            self.evaluate()
        } else {
            let mut max = i32::MIN;
            for m in self.generate_moves().iter() {
                self.make(m); //Assume the move is legal. This must be ensured by genmoves.
                let score = -self.min_max_helper(depth - 1);
                self.unmake(m);
                if score > max {
                    max = score;
                }
            }
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

        let to_file = Board::file_letter_to_number(chars.next().unwrap()) as i8;
        let to_rank = chars.next().unwrap().to_digit(10).unwrap() as i8;

        match self
            .board
            .move_piece(from_rank, from_file, to_rank, to_file)
        {
            Ok(()) => self.end_ply(),
            Err(e) => {
                eprintln!("{}", e.to_string().red());
                return;
            }
        }
    }

    fn end_ply(&mut self) {
        self.turn = self.turn.opposite();
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

    fn generate_moves(&self) -> Vec<Move> {
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
                                .pseudo_legal_moves(rank, file, MoveType::DefaultOrAttack)
                                .unwrap(),
                        );
                    }
                }
            }
        }
        v
    }

    fn make(&self, m: &Move) {
        todo!()
        //Add the moves to self.moves, and if attacking move, add the taken piece to taken
    }

    fn unmake(&self, m: &Move) {
        todo!()
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
#[derive(PartialEq)]
pub struct Move {
    from: Field,
    to: Field,
    move_type: MoveType,
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

    ///Check for piece move legality (attack or default) & obstruction.
    /// Does not yet check for checks or pins.
    fn move_piece(
        &mut self,
        from_rank: i8,
        from_file: i8,
        to_rank: i8,
        to_file: i8,
    ) -> Result<(), Error> {
        let square = match self.at(from_rank, from_file) {
            Square::Full(p) => Square::Full(p),
            _ => return Err(Error::NoPieceOnField(Field::new(from_rank, from_file))),
        };

        if !self
            .pseudo_legal_move_fields(from_rank, from_file, MoveType::DefaultOrAttack)?
            .contains(&Field::new(to_rank, to_file))
        {
            return Err(Error::BadMoveTarget(Field::new(to_rank, to_file)));
        }

        self.set(from_rank, from_file, Square::Empty);
        self.set(to_rank, to_file, square);
        Ok(())
    }

    pub fn pseudo_legal_moves(
        &self,
        rank: i8,
        file: i8,
        move_type: MoveType,
    ) -> Result<Vec<Move>, Error> {
        let fields = self.pseudo_legal_move_fields(rank, file, move_type)?;
        Ok(fields
            .into_iter()
            .map(|field| Move::new(Field::new(rank, file), field, move_type))
            .collect())
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
    fn pseudo_legal_move_fields(
        &self,
        rank: i8,
        file: i8,
        move_type: MoveType,
    ) -> Result<Vec<Field>, Error> {
        match self.at(rank, file) {
            Square::Full(p) => match p.kind {
                PieceKind::Pawn => {
                    Ok(self.pseudo_legal_pawn_move_fields(p.color, rank, file, move_type))
                }
                PieceKind::Knight => {
                    Ok(self.pseudo_legal_knight_move_fields(p.color, rank, file, move_type))
                }
                PieceKind::King => {
                    Ok(self.pseudo_legal_king_move_fields(p.color, rank, file, move_type))
                }
                PieceKind::Rook => {
                    Ok(self.pseudo_legal_rook_move_fields(p.color, rank, file, move_type))
                }
                PieceKind::Bishop => {
                    Ok(self.pseudo_legal_bishop_move_fields(p.color, rank, file, move_type))
                }
                PieceKind::Queen => {
                    Ok(self.pseudo_legal_queen_move_fields(p.color, rank, file, move_type))
                }
            },
            _ => Err(Error::NoPieceOnField(Field::new(rank, file))),
        }
    }

    fn pseudo_legal_pawn_move_fields(
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
                //This is the worst sin I have ever committed.
                //Once I have time, definitely refactor this so it's not a copy of
                //both above cases!
                MoveType::DefaultOrAttack => {
                    let mut v = match self.at(rank - 1, file) {
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
                    };
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
                MoveType::DefaultOrAttack => {
                    let mut v = match self.at(rank + 1, file) {
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
                    };

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

    fn pseudo_legal_knight_move_fields(
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
            MoveType::Default => self.filter_free(v),
            MoveType::Attack => {
                let opposite = color.opposite();
                self.filter_opponent(v, opposite)
            }
            MoveType::DefaultOrAttack => {
                let opposite = color.opposite();
                self.filter_free_or_opponent(v, opposite)
            }
        }
    }

    ///This is quite scuffed at the moment since king moves are quite useless
    /// without checking for checks...
    /// todo: enemy checks
    fn pseudo_legal_king_move_fields(
        &self,
        color: PieceColor,
        rank: i8,
        file: i8,
        move_type: MoveType,
    ) -> Vec<Field> {
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
        match move_type {
            MoveType::Default => self.filter_free(v),
            MoveType::Attack => {
                let opposite = color.opposite();
                self.filter_opponent(v, opposite)
            }
            MoveType::DefaultOrAttack => {
                let opposite = color.opposite();
                self.filter_free_or_opponent(v, opposite)
            }
        }
    }

    ///todo: movetype attack wird noch ignoriert
    fn pseudo_legal_rook_move_fields(
        &self,
        color: PieceColor,
        rank: i8,
        file: i8,
        move_type: MoveType,
    ) -> Vec<Field> {
        //Upwards
        let mut v = vec![];
        for i in rank + 1..=8 {
            if self.insert_or_break_loop(color, i, file, move_type, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Downwards
        for i in (1..=rank - 1).rev() {
            if self.insert_or_break_loop(color, i, file, move_type, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Right
        for j in file + 1..=8 {
            if self.insert_or_break_loop(color, rank, j, move_type, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Left
        for j in (1..=file - 1).rev() {
            if self.insert_or_break_loop(color, rank, j, move_type, &mut v) == BreakLoop::True {
                break;
            }
        }
        v
    }

    fn pseudo_legal_bishop_move_fields(
        &self,
        color: PieceColor,
        rank: i8,
        file: i8,
        move_type: MoveType,
    ) -> Vec<Field> {
        let mut v = vec![];
        let mut diff = 1;

        //Todo: bekommt hier irgendwie eine alte version
        //des boards. Vielleicht muss ich das mehr mit
        //Referenzen rumpassen?

        //Up right
        while rank + diff <= 8 && file + diff <= 8 {
            if self.insert_or_break_loop(color, rank + diff, file + diff, move_type, &mut v)
                == BreakLoop::True
            {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Up left
        while rank + diff <= 8 && file - diff >= 1 {
            if self.insert_or_break_loop(color, rank + diff, file - diff, move_type, &mut v)
                == BreakLoop::True
            {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Down right
        while rank - diff >= 1 && file + diff <= 8 {
            if self.insert_or_break_loop(color, rank - diff, file + diff, move_type, &mut v)
                == BreakLoop::True
            {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Down left
        while rank - diff >= 1 && file - diff >= 1 {
            if self.insert_or_break_loop(color, rank - diff, file - diff, move_type, &mut v)
                == BreakLoop::True
            {
                break;
            }
            diff += 1;
        }
        v
    }

    ///Inefficiency at its peak
    fn pseudo_legal_queen_move_fields(
        &self,
        color: PieceColor,
        rank: i8,
        file: i8,
        move_type: MoveType,
    ) -> Vec<Field> {
        let mut v = self.pseudo_legal_rook_move_fields(color, rank, file, move_type);
        v.extend(self.pseudo_legal_bishop_move_fields(color, rank, file, move_type));
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
        rank: i8,
        file: i8,
        move_type: MoveType,
        v: &mut Vec<Field>,
    ) -> BreakLoop {
        match self.at(rank, file) {
            Square::Empty => {
                v.push(Field::new(rank, file));
            }
            Square::Full(p) => {
                match move_type {
                    MoveType::Attack | MoveType::DefaultOrAttack => {
                        if p.color == color.opposite() {
                            v.push(Field::new(rank, file));
                        }
                    }
                    MoveType::Default => (),
                }
                return BreakLoop::True;
            }
            Square::Padding => {
                panic!("invalid argument for insert_or_break_loop")
            }
        }
        BreakLoop::False
    }

    fn filter_free(&self, v: Vec<Field>) -> Vec<Field> {
        v.into_iter()
            .filter(|field| matches!(self.at(field.rank, field.file), Square::Empty))
            .collect()
    }

    ///For non-sliding moves: Return only those fields that are either empty
    /// or occuppied by a piece of color opposite.
    /// This function is required for non-sliding moves and takes as input
    /// a vector of all potential moves based on the piece's kind.
    fn filter_opponent(&self, v: Vec<Field>, opposite: PieceColor) -> Vec<Field> {
        v.into_iter()
            .filter(|field| {
                if let Square::Full(p) = self.at(field.rank, field.file) {
                    p.color == opposite
                } else {
                    false
                }
            })
            .collect()
    }

    fn filter_free_or_opponent(&self, v: Vec<Field>, opposite: PieceColor) -> Vec<Field> {
        v.into_iter()
            .filter(|field| {
                matches!(self.at(field.rank, field.file), Square::Empty)
                    || if let Square::Full(p) = self.at(field.rank, field.file) {
                        p.color == opposite
                    } else {
                        false
                    }
            })
            .collect()
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

    pub fn draw_pseudo_legal_moves(&self, pos_rank: i8, pos_file: i8, move_type: MoveType) {
        let v = self
            .pseudo_legal_moves(pos_rank, pos_file, move_type)
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

                let tile = if v.contains(&Move::new(
                    Field::new(pos_rank, pos_file),
                    Field::new(rank, file),
                    MoveType::DefaultOrAttack,
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
    }

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
    }
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

#[derive(Clone, Copy, PartialEq)]
pub enum MoveType {
    Default,
    Attack,
    DefaultOrAttack,
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
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
