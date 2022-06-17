//#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
mod evaluation;
mod movegen;
mod search;
mod zobrist;

use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use colored::Colorize;
use evaluation::EvaluationFlag;
use ndarray::prelude::*;
use zobrist::*;

//Default starting position
const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq";

//Sicilian after opening
//const STARTING_POSITION: &str = "rnbqkbnr/1p2pppp/p2p4/2p5/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq";

//Queens indian
//const STARTING_POSITION: &str = "rn1qk2r/p1ppbppp/bp2pn2/8/2PP4/1P3NP1/P2BPP1P/RN1QKB1R w KQkq";

//Checkmate in 1
//const STARTING_POSITION: &str = "1k6/ppp3Q1/8/8/8/8/6K1/8 w";

//Mate in 3
//const STARTING_POSITION: &str = "7R/2N1P3/8/8/8/8/k6K/8 w";

//21 ply search should give king to b1. Test transposition table. Fine#70
//const STARTING_POSITION: &str = "8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w";

//Mate in 3: HAKMEM70 (fails until promotion to knight is generated)
//const STARTING_POSITION: &str = "5B2/6P1/1p6/8/1N6/kP6/2K5/8 w";

const TRANSPOSITION_TABLE_SIZE: usize = 18_000_041;

#[derive(Clone)]
pub struct BoardState {
    pub board: Board,
    pub turn: PieceColor,
    ply: u16,
    pub castling_rights: CastlingRights,
    zobrist: ZobristState,
    en_passant: Option<Field>, //only used on the first move!
    moves: Vec<Move>,
    pub taken: Vec<Piece>,
    pub hash_history: Vec<u64>,
}

impl BoardState {
    pub fn en_passant(&self) -> Option<Field> {
        if let Some(last) = self.moves.last() {
            if last.piece.kind == PieceKind::Pawn && (last.from.rank - last.to.rank).abs() == 2 {
                if last.piece.color == PieceColor::White {
                    return Some(Field::new(last.from.rank + 1, last.from.file));
                } else {
                    return Some(Field::new(last.from.rank - 1, last.from.file));
                }
            }
            None
        } else {
            //On the first move we want to return the en_passant value given in the FEN string.
            self.en_passant
        }
    }

    pub fn new() -> Self {
        Self::from_fen(STARTING_POSITION).unwrap()
    }

    fn absolute_eval(&self, rel_eval: i32) -> i32 {
        if self.turn == PieceColor::White {
            rel_eval
        } else {
            -rel_eval
        }
    }

    ///Just to make naming consistent
    fn relative_eval(&self, abs_eval: i32) -> i32 {
        self.absolute_eval(abs_eval)
    }

    ///Get index to transposition table, depending on TRANSPOSITION_TABLE_SIZE
    ///
    /// Zobrist keys are 64 bits. But to index the hashmap we use a smaller subset of this and accept hash collisions.
    /// Caller must confirm that the 64 bit zobrist keys match before using any value stored in the hashmap.
    fn transposition_table_index(&self) -> u32 {
        (self.zobrist.hash % TRANSPOSITION_TABLE_SIZE as u64) as u32
    }

    ///This only returns Some if entry.key == self.zobrist.hash
    pub fn get_transposition_entry<'a>(
        &self,
        transposition_table: &'a HashMap<u32, TranspositionEntry>,
    ) -> Option<&'a TranspositionEntry> {
        if let Some(entry) = transposition_table.get(&self.transposition_table_index()) {
            if entry.zobrist_key == self.zobrist.hash {
                return Some(entry);
            }
        }
        None
    }

    #[deprecated]
    fn tt_table_test_get_transposition_entry<'a>(
        &self,
        transposition_table: &'a HashMap<u32, TranspositionEntry>,
    ) -> Option<&'a TranspositionEntry> {
        if let Some(entry) = transposition_table.get(&self.transposition_table_index()) {
            if entry.zobrist_key == self.zobrist.hash {
                return Some(entry);
            }
        }
        None
    }

    ///Returns Some if there is an entry for the current hash that
    /// <ul>
    /// <li>matches the full zobrist key</li>
    /// <li>has entry.depth of at least depth</li>
    /// <li>matches the given flag</li>
    /// </ul>
    ///
    /// Note this returns the exact entry, so in particular the evaluation is stored absolutely.
    fn get_matching_transposition_entry<'a>(
        &self,
        depth: u32,
        flag: EvaluationFlag,
        transposition_table: &'a HashMap<u32, TranspositionEntry>,
    ) -> Option<&'a TranspositionEntry> {
        // eprintln!("start get_matching_tt");
        if let Some(entry) = self.get_transposition_entry(transposition_table) {
            if entry.depth >= depth && entry.flag == flag {
                // eprintln!("return matching entry: {:?}", entry);
                return Some(entry);
            }
        }
        None
    }

    ///Scheme: Keep deeper. If same depth, keep exact over lowerbound over upperbound.
    /// todo: vielleicht auch berücksichtigen, ob new einen move speichern würde
    ///
    /// <b> Evaluation must be positive for white!</b>
    fn update_transposition_table(
        &self,
        transposition_table: &mut HashMap<u32, TranspositionEntry>,
        new_entry: TranspositionEntry,
    ) {
        // eprintln!("start update_tt");
        if let Some(old_entry) = self.get_transposition_entry(transposition_table) {
            //Always keep deeper entry
            if old_entry.depth > new_entry.depth {
                return;
            }
            //If same depth, keep the one with the better flag.
            if old_entry.depth == new_entry.depth && old_entry.flag >= new_entry.flag {
                return;
            }
        }
        // eprintln!("tt updated: {:?}", new_entry);
        transposition_table.insert(self.transposition_table_index(), new_entry);
    }

    pub fn threatened(&self, victim_color: PieceColor, field: Field) -> bool {
        //Kinght Moves

        for m in self
            .board
            .pseudo_legal_knight_moves(victim_color, field, true)
        {
            let Field { rank, file } = m.to;
            if let Square::Full(p) = self.board.at(rank, file) {
                if matches!(p.kind, PieceKind::Knight) {
                    return true;
                }
            }
        }

        //Rooks/Queens

        for m in self
            .board
            .pseudo_legal_rook_moves(victim_color, field, true, false)
        {
            let Field { rank, file } = m.to;
            if let Square::Full(p) = self.board.at(rank, file) {
                if matches!(p.kind, PieceKind::Rook) || matches!(p.kind, PieceKind::Queen) {
                    return true;
                }
            }
        }

        //Bishops/Queens

        for m in self
            .board
            .pseudo_legal_bishop_moves(victim_color, field, true, false)
        {
            let Field { rank, file } = m.to;
            if let Square::Full(p) = self.board.at(rank, file) {
                if matches!(p.kind, PieceKind::Bishop) || matches!(p.kind, PieceKind::Queen) {
                    return true;
                }
            }
        }

        //Pawns

        /*Notice that we don't generate en passant captures at this point! Doing so leads
        to very weird behaviour if the white king is on d5 and the last move was c7c5, so c6 is
        and en_passant square. Then Kd5c6 would be a pseudolegal move, thus the program would think
        the king is under attack from a pawn on c6, even though there is nothing there. The program would
        incorrectly conclude that the current position is check. That would be sad.*/

        for m in self.pseudo_legal_pawn_moves(victim_color, field, true, true) {
            let Field { rank, file } = m.to;
            if let Square::Full(p) = self.board.at(rank, file) {
                if matches!(p.kind, PieceKind::Pawn) {
                    return true;
                }
            }
        }

        false
    }

    ///Returns whether color is in check
    pub fn check(&self, color: PieceColor) -> bool {
        match color {
            PieceColor::White => self.threatened(color, self.board.white_king),
            PieceColor::Black => self.threatened(color, self.board.black_king),
        }
    }

    ///<b>Precondition: zero legal moves for winning_color.opposite()</b>
    pub fn checkmate_given_zero_moves(&self, winning_color: PieceColor) -> bool {
        self.check(winning_color.opposite())
    }

    ///True if the last move leads to a threefold repetition. Might return true even though there is no threefold repetition.
    pub fn threefold_repetition(&self) -> bool {
        let last = self.hash_history.last().unwrap();
        self.hash_history.iter().filter(|&n| n == last).count() >= 3
    }

    ///Only called in make. To unmake the hash, we just pop the last value from the hash history.
    /// old_en_passant: needs the state.en_passant() from before make() was called.
    pub fn make_zobrist(
        &mut self,
        m: &Move,
        moving_piece: Piece,
        changed_castling_rights: &ChangedCastlingRights,
        old_en_passant: Option<Field>,
    ) {
        //Put moving piece on new square and remove moving piece.
        //In a promotion move, moving_piece is a queen, which is important when unmaking since we have to place a pawn
        //to the from position.

        match m.promotion {
            //Might be a promotion move.
            Some(promo) => {
                self.zobrist.change_piece(m.to, promo);
                self.zobrist
                    .change_piece(m.from, Piece::pawn(moving_piece.color));
            }
            None => {
                self.zobrist.change_piece(m.to, moving_piece);
                self.zobrist.change_piece(m.from, moving_piece);
            }
        }

        match m.move_type {
            MoveType::Default => {}
            MoveType::Capture => {
                //Remove taken piece
                if let Square::Full(target) = self.board.at(m.to.rank, m.to.file) {
                    self.zobrist.change_piece(m.to, target);
                } else {
                    self.zobrist.change_piece(
                        m.get_en_passant_taken().unwrap(),
                        Piece::pawn(moving_piece.color.opposite()),
                    )
                }
            }
            //Remove and put the rook if castling
            MoveType::CastleKingside => match self.turn {
                PieceColor::White => {
                    self.zobrist
                        .change_piece(Field::new(1, 8), Piece::rook(PieceColor::White));
                    self.zobrist
                        .change_piece(Field::new(1, 6), Piece::rook(PieceColor::White));
                }
                PieceColor::Black => {
                    self.zobrist
                        .change_piece(Field::new(8, 8), Piece::rook(PieceColor::Black));
                    self.zobrist
                        .change_piece(Field::new(8, 6), Piece::rook(PieceColor::Black));
                }
            },
            MoveType::CastleQueenside => match self.turn {
                PieceColor::White => {
                    self.zobrist
                        .change_piece(Field::new(1, 1), Piece::rook(PieceColor::White));
                    self.zobrist
                        .change_piece(Field::new(1, 4), Piece::rook(PieceColor::White));
                }
                PieceColor::Black => {
                    self.zobrist
                        .change_piece(Field::new(8, 1), Piece::rook(PieceColor::Black));
                    self.zobrist
                        .change_piece(Field::new(8, 4), Piece::rook(PieceColor::Black));
                }
            },
        }

        //Remove castling rights

        if changed_castling_rights.white_king {
            self.zobrist
                .change_castling_rights(MoveType::CastleKingside, PieceColor::White);
        }
        if changed_castling_rights.white_queen {
            self.zobrist
                .change_castling_rights(MoveType::CastleQueenside, PieceColor::White);
        }
        if changed_castling_rights.black_king {
            self.zobrist
                .change_castling_rights(MoveType::CastleKingside, PieceColor::Black);
        }
        if changed_castling_rights.black_queen {
            self.zobrist
                .change_castling_rights(MoveType::CastleQueenside, PieceColor::Black);
        }

        //Undo en_passant from previous move and do en_passant for current move
        if let Some(ep) = old_en_passant {
            self.zobrist.change_en_passant(ep);
        }
        if let Some(ep) = self.en_passant() {
            self.zobrist.change_en_passant(ep);
        }

        //Change turn

        self.zobrist.change_black_to_move();
    }

    ///Find the best move for the player whose turn it is by checking all possible moves
    /// 'depth' plies into the future, always assuming the opponent answers with his own
    /// best move.
    ///
    /// TODO: checkmate is not a valid option! should give infinite score, so that
    /// this variant is always taken!
    /*pub fn nega_max(&mut self, depth: i32) -> Move {
        let mut best_move = Move::new(Field::new(-2, -2), Field::new(-2, -2), MoveType::Default);
        let mut max_value = i32::MIN;
        for m in self.generate_moves() {
            self.make(m);
            let mut score = i32::MIN;
            if !self.check(self.turn.opposite()) {
                score = -self.nega_max_helper(depth - 1);
            }
            self.unmake();
            if score > max_value {
                best_move = m;
                max_value = score;
            }
        }
        if max_value == i32::MIN {
            panic!("checkmate! (or stalemate)");
        }
        if max_value == i32::MAX {
            eprintln!("FOUND A CHECKMATE SEQUENCE? with {}", best_move);
        }
        best_move
    }

    ///For every legal move in the current position, this function calls make,
    /// then calls itself recursively, and finally calls unmake.
    ///
    /// Depth is how many recursive calls it should do.
    ///
    /// We assume the opponent always plays the best move (just like us). That's why
    /// we can just use the same function for both cases.
    ///
    ///Returns the evaluation of the board after playing the best legal move for the
    /// player whose turn it currently is.
    fn nega_max_helper(&mut self, depth: i32) -> i32 {
        if depth == 0 {
            self.evaluate()
        } else {
            let mut max = i32::MIN;
            for m in self.generate_moves() {
                self.make(m);

                /*
                It's now the opponent's turn. Since we assume he plays *his* strongest
                move (worst case), we use whatever this function returns right now.
                We negate it because this function returns evaluations relative to the
                player whose turn it currently is. At the time of this recursive call,
                it's the opponent's turn, but in the main body of this function
                we want to know the score relativ to the player whose turn it is
                at the beginning.
                */
                let mut score = i32::MIN;

                //If a move lands the current color in check, don't consider the move and
                //variants resulting from it
                if !self.check(self.turn.opposite()) {
                    score = -self.nega_max_helper(depth - 1);
                }

                self.unmake();

                //It is now the original player's turn again.

                //We want to maximize the evaluation of the board after the next 'depth'
                //moves from our own perspective.
                if score > max {
                    max = score;
                }
            }
            max
        }
    }*/

    ///Move piece by string like "e2e4". Checks for legality.
    pub fn move_by_str(&mut self, arg: &str) -> Result<(), Error> {
        println!("\nTry to process input: {}", arg);

        let mut chars = arg.chars();

        let from = Self::parse_field(&mut chars)?;

        if let Square::Full(p) = self.board.at(from.rank, from.file) {
            if p.color != self.turn {
                eprintln!("{} to move!", self.turn.to_string().red());
                return Err(Error::WrongColor(from));
            }
        } else {
            return Err(Error::NoPieceOnField(from));
        }

        //From here on we can be sure that it's the correct side to move and the square is
        //actually full.
        let to = Self::parse_field(&mut chars)?;

        let mut v = self
            .pseudo_legal_moves(from, false)
            .unwrap()
            .into_iter()
            .find(|m| m.from == from && m.to == to);

        if v.is_none() {
            v = self
                .castling_moves()
                .into_iter()
                .find(|m| m.from == from && m.to == to);
        }

        if let Some(m) = v {
            self.make(&m);
            if self.check(self.turn.opposite()) {
                println!("You're in check!");
                self.unmake();
                return Err(Error::BadMoveTarget(to));
            }
            Ok(())
        } else {
            Err(Error::BadMoveTarget(to))
        }
    }

    pub fn draw(&self, show_last: bool) {
        self.draw_board(show_last);
        self.print_state_info();
    }

    fn print_state_info(&self) {
        println!("Turn: {:?}", self.turn);
        println!("Ply: {:?}", self.ply);
        println!(
            "White king: {:?}",
            self.castling_rights.white_king_castle_lost_ply.unwrap_or(0)
        );
        println!(
            "White queen: {:?}",
            self.castling_rights
                .white_queen_castle_lost_ply
                .unwrap_or(0)
        );
        println!(
            "Black king: {:?}",
            self.castling_rights.black_king_castle_lost_ply.unwrap_or(0)
        );
        println!(
            "Black queen: {:?}",
            self.castling_rights
                .black_queen_castle_lost_ply
                .unwrap_or(0)
        );
        //println!("En passant square: {:?}", self.en_passant);

        /*todo!(
            "hier sollte ich minmax aufrufen, weil evaluate die position nur statisch
        auswertet. ich will aber die moves der zukunft auch berücksichtigen."
        );
        println!("EVALUATION: {}", self.evaluate());*/
    }

    ///Draw chess board using unicode characters.
    /// todo: could draw pseudolegal moves for a specific piece (maybe in
    /// another function)
    pub fn draw_board(&self, show_prev: bool) {
        for rank in (1..9).rev() {
            print!("{}", rank);
            for file in 1..9 {
                let square = self.board.at(rank, file);
                //eprintln!("{},{}: {:?}", rank, file, square);
                let field = Field::new(rank, file);
                let tile = match square.unicode_str() {
                    Some(s) => match square.color() {
                        Some(PieceColor::White) => s.white(),
                        Some(PieceColor::Black) => s.black(),
                        _ => s.normal(),
                    },
                    None => continue,
                };

                let tile = if (rank + file) % 2 == 0 {
                    // tile.on_black() //black
                    tile.on_truecolor(200, 200, 200) //black
                } else {
                    tile.on_truecolor(230, 230, 230) //white
                };

                let tile = if show_prev {
                    if let Some(last) = self.moves.last() {
                        if last.from == field || last.to == field {
                            tile.on_truecolor(122, 156, 70)
                        } else {
                            tile
                        }
                    } else {
                        tile
                    }
                } else {
                    tile
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

    ///Consumes one trailing whitespace
    fn parse_board_fen(chars: &mut std::str::Chars) -> Result<Board, Error> {
        let mut board = Array2::<Square>::default((12, 12));
        let mut rank = 8;
        let mut file = 1;
        let mut black_king = None;
        let mut white_king = None;

        //Padding around the board
        for i in 0..12 {
            for j in 0..12 {
                if (2..=9).contains(&i) {
                    board[[i, 0]] = Square::Padding;
                    board[[i, 1]] = Square::Padding;
                    board[[i, 10]] = Square::Padding;
                    board[[i, 11]] = Square::Padding;
                } else {
                    board[[i, j]] = Square::Padding;
                }
            }
        }

        for c in chars.by_ref() {
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
                'k' => {
                    board[[i, j]] = Square::Full(Piece::king(PieceColor::Black));
                    black_king = Some(Field::new(rank, file));
                }
                'P' => board[[i, j]] = Square::Full(Piece::pawn(PieceColor::White)),
                'B' => board[[i, j]] = Square::Full(Piece::bishop(PieceColor::White)),
                'N' => board[[i, j]] = Square::Full(Piece::knight(PieceColor::White)),
                'R' => board[[i, j]] = Square::Full(Piece::rook(PieceColor::White)),
                'Q' => board[[i, j]] = Square::Full(Piece::queen(PieceColor::White)),
                'K' => {
                    board[[i, j]] = Square::Full(Piece::king(PieceColor::White));
                    white_king = Some(Field::new(rank, file));
                }
                ' ' => break,
                _ => return Err(Error::Parse),
            }
            if !(1..=8).contains(&rank) || !(1..=8).contains(&file) {
                println!("rank={}, file={}", rank, file);
                return Err(Error::Parse);
            }
            file += 1;
        }

        Ok(Board {
            board,
            white_king: white_king.ok_or(Error::Parse)?,
            black_king: black_king.ok_or(Error::Parse)?,
        })
    }

    ///Consume one trailing whitespace
    fn parse_turn_fen(chars: &mut std::str::Chars) -> Result<PieceColor, Error> {
        let mut turn = PieceColor::White;

        if let Some(c) = chars.by_ref().next() {
            turn = match c {
                'w' | 'W' => PieceColor::White,
                'b' | 'B' => PieceColor::Black,
                _ => return Err(Error::Parse),
            }
        };
        chars.next();
        Ok(turn)
    }

    ///Consumes one trailing whitespace
    /// Standard: Assume no castling rights.
    fn parse_castling_rights_fen(
        chars: &mut std::str::Chars,
        board: &Board,
    ) -> Result<CastlingRights, Error> {
        let mut white_queen_castle_lost_ply = Some(0);
        let mut black_queen_castle_lost_ply = Some(0);
        let mut white_king_castle_lost_ply = Some(0);
        let mut black_king_castle_lost_ply = Some(0);
        let white_king = board.white_king;
        let black_king = board.black_king;

        for c in chars.by_ref() {
            match c {
                'K' => {
                    assert!(white_king == Field::new(1, 5));
                    assert!(board.piece_at(Field::new(1, 8), Piece::rook(PieceColor::White)));
                    white_king_castle_lost_ply = None;
                }
                'Q' => {
                    assert!(white_king == Field::new(1, 5));
                    assert!(board.piece_at(Field::new(1, 1), Piece::rook(PieceColor::White)));
                    white_queen_castle_lost_ply = None;
                }
                'k' => {
                    assert!(black_king == Field::new(8, 5));
                    assert!(board.piece_at(Field::new(8, 8), Piece::rook(PieceColor::Black)));
                    black_king_castle_lost_ply = None;
                }
                'q' => {
                    assert!(black_king == Field::new(8, 5));
                    assert!(board.piece_at(Field::new(8, 1), Piece::rook(PieceColor::Black)));
                    black_queen_castle_lost_ply = None;
                }
                ' ' => break,
                '-' => {
                    chars.next();
                    break;
                }
                _ => return Err(Error::Parse),
            }
        }
        Ok(CastlingRights {
            white_queen_castle_lost_ply,
            black_queen_castle_lost_ply,
            white_king_castle_lost_ply,
            black_king_castle_lost_ply,
        })
    }

    ///Consume one trailing whitespace
    fn parse_en_passant_fen(chars: &mut std::str::Chars) -> Option<Field> {
        let field = Self::parse_field(chars);
        chars.next();
        field.ok()
    }

    ///Does not consume trailing whitespace  
    fn parse_field(chars: &mut std::str::Chars) -> Result<Field, Error> {
        let file = Board::file_letter_to_number(chars.next().ok_or(Error::Parse)?)? as i8;
        let rank = chars
            .next()
            .ok_or(Error::Parse)?
            .to_digit(10)
            .ok_or(Error::Parse)? as i8;
        Ok(Field::new(rank, file))
    }

    ///Standard: Assume no castling rights
    fn from_fen(fen: &str) -> Result<BoardState, Error> {
        let chars = &mut fen.chars();

        let board = Self::parse_board_fen(chars)?;

        let turn = Self::parse_turn_fen(chars)?;

        let castling_rights = Self::parse_castling_rights_fen(chars, &board)?;

        let en_passant = Self::parse_en_passant_fen(chars);

        let zobrist = ZobristState::from(&board, turn, &castling_rights, en_passant);

        let hashes = vec![zobrist.hash];

        //eprintln!("{:?}", board.board);

        Ok(Self {
            board,
            turn,
            ply: 1,
            castling_rights,
            zobrist,
            en_passant,
            moves: vec![],
            taken: vec![],
            hash_history: hashes,
        })
    }

    pub fn print_all_moves(&self) {
        let mut ply = 2;
        for m in self.moves.iter() {
            if ply % 2 == 0 {
                print!("{}. ", ply / 2);
            }
            if m.move_type == MoveType::CastleKingside {
                print!("O-O ");
            } else if m.move_type == MoveType::CastleQueenside {
                print!("O-O-O ");
            } else {
                print!("{} ", m);
            }
            ply += 1;
        }
    }
}

#[derive(Clone, Debug)]
pub struct CastlingRights {
    pub white_queen_castle_lost_ply: Option<u16>,
    pub black_queen_castle_lost_ply: Option<u16>,
    pub white_king_castle_lost_ply: Option<u16>,
    pub black_king_castle_lost_ply: Option<u16>,
}

impl Default for BoardState {
    fn default() -> Self {
        Self::new()
    }
}

///If move_type==Castle, from must contain the king's position!
/// to is ignored.
/// todo: use piece in more places instead of calling at() with the from field.
/// En passant: to is the position the moving pawn moved to. The position where the captured pawn was must be calculated.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Move {
    pub from: Field,
    pub to: Field,
    piece: Piece,
    move_type: MoveType,
    promotion: Option<Piece>,
    en_passant: bool,
}

impl Move {
    const fn new(from: Field, to: Field, piece: Piece, move_type: MoveType) -> Self {
        Self {
            from,
            to,
            piece,
            move_type,
            promotion: None,
            en_passant: false,
        }
    }

    ///Generate a new move which may be a promotion (or not).
    const fn promotion(
        from: Field,
        to: Field,
        piece: Piece,
        move_type: MoveType,
        promo: Option<Piece>,
    ) -> Self {
        Self {
            from,
            to,
            piece,
            move_type,
            promotion: promo,
            en_passant: false,
        }
    }

    ///Generate a new en passant capture move.
    const fn en_passant(from: Field, color: PieceColor, to: Field) -> Self {
        Self {
            from,
            to,
            piece: Piece::pawn(color),
            move_type: MoveType::Capture,
            promotion: None,
            en_passant: true,
        }
    }

    fn get_en_passant_taken(&self) -> Option<Field> {
        if !self.en_passant {
            return None;
        }

        if self.piece.color == PieceColor::White {
            Some(Field::new(self.to.rank - 1, self.to.file))
        } else {
            Some(Field::new(self.to.rank + 1, self.to.file))
        }
    }
}

impl PartialOrd for Move {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
        use MoveType::*;
        match (self.move_type, other.move_type) {
            (Capture, Capture) => self.piece.partial_cmp(&other.piece),
            (Capture, _) => Some(Less),
            (_, Capture) => Some(Greater),
            _ => match (self.promotion, other.promotion) {
                (Some(_), None) => Some(Less),
                (None, Some(_)) => Some(Greater),
                _ => self.piece.partial_cmp(&other.piece),
            },
        }
    }
}

impl Ord for Move {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.piece.letter(), self.from, self.to)
    }
}

#[derive(Clone)]
pub struct Board {
    board: Array2<Square>,
    white_king: Field,
    black_king: Field,
}

impl Board {
    ///Readonly!
    /// Out-of-bounds are supported as far as padding goes. That is, rank 0 and rank -1
    /// would be padding rows, but rank -2 would panic.
    ///
    /// <b>Precondition: -1 <= rank, file <= 10
    pub fn at(&self, rank: i8, file: i8) -> Square {
        let (i, j) = Board::to_board(rank, file);
        self.board[[i, j]]
    }

    /// <b>Precondition: -1 <= rank, file <= 10
    fn set(&mut self, rank: i8, file: i8, val: Square) {
        let (i, j) = Board::to_board(rank, file);
        self.board[[i, j]] = val;
    }

    fn empty(&self, rank: i8, file: i8) -> bool {
        matches!(self.at(rank, file), Square::Empty)
    }

    ///Example: Converts a to 1.
    const fn file_letter_to_number(file: char) -> Result<usize, Error> {
        match file {
            'a' => Ok(1),
            'b' => Ok(2),
            'c' => Ok(3),
            'd' => Ok(4),
            'e' => Ok(5),
            'f' => Ok(6),
            'g' => Ok(7),
            'h' => Ok(8),
            _ => Err(Error::Parse),
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

    ///Returns true if piece is on that field.
    fn piece_at(&self, field: Field, piece: Piece) -> bool {
        if let Square::Full(p) = self.at(field.rank, field.file) {
            return p == piece;
        }
        false
    }
}

#[derive(Eq, PartialEq, PartialOrd, Clone, Copy, Debug)]
pub enum PieceKind {
    Pawn,
    Bishop,
    Knight,
    Rook,
    Queen,
    King,
}

#[derive(Eq, PartialEq, Clone, Copy, PartialOrd, Debug)]
pub struct Piece {
    pub kind: PieceKind,
    pub color: PieceColor,
}

impl Piece {
    const fn pawn(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Pawn,
            color,
        }
    }

    const fn bishop(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Bishop,
            color,
        }
    }

    const fn knight(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Knight,
            color,
        }
    }

    const fn rook(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Rook,
            color,
        }
    }

    const fn queen(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::Queen,
            color,
        }
    }

    const fn king(color: PieceColor) -> Self {
        Self {
            kind: PieceKind::King,
            color,
        }
    }

    fn is_line(&self) -> bool {
        self.kind == PieceKind::Rook || self.kind == PieceKind::Queen
    }

    fn is_diagonal(&self) -> bool {
        self.kind == PieceKind::Bishop || self.kind == PieceKind::Queen
    }

    fn unicode_str(&self) -> &str {
        if self.color == PieceColor::White {
            match self.kind {
                PieceKind::Pawn => "♙ ",
                PieceKind::Bishop => "♗ ",
                PieceKind::Knight => "♘ ",
                PieceKind::Rook => "♖ ",
                PieceKind::Queen => "♕ ",
                PieceKind::King => "♔ ",
            }
        } else {
            match self.kind {
                PieceKind::Pawn => "♟ ",
                PieceKind::Bishop => "♝ ",
                PieceKind::Knight => "♞ ",
                PieceKind::Rook => "♜ ",
                PieceKind::Queen => "♛ ",
                PieceKind::King => "♚ ",
            }
        }
    }

    pub fn index(&self) -> usize {
        let i = match self.kind {
            PieceKind::Pawn => 0,
            PieceKind::Bishop => 1,
            PieceKind::Knight => 2,
            PieceKind::Rook => 3,
            PieceKind::Queen => 4,
            PieceKind::King => 5,
        };

        i + if self.color == PieceColor::White {
            0
        } else {
            6
        }
    }

    pub fn letter(&self) -> &str {
        match self.kind {
            PieceKind::Pawn => "",
            PieceKind::Bishop => "B",
            PieceKind::Knight => "N",
            PieceKind::Rook => "R",
            PieceKind::Queen => "Q",
            PieceKind::King => "K",
        }
    }
}

///None means no move has been stored
///
/// positive eval <=> white advantage
#[derive(Debug, Clone)]
pub struct TranspositionEntry {
    zobrist_key: u64,
    depth: u32,
    eval: i32,
    flag: EvaluationFlag,
    best_move: Option<Move>,
}

impl TranspositionEntry {
    pub fn new(
        zobrist_key: u64,
        depth: u32,
        eval: i32,
        flag: EvaluationFlag,
        best_move: Option<Move>,
    ) -> Self {
        Self {
            zobrist_key,
            depth,
            eval,
            flag,
            best_move,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Square {
    Full(Piece),
    Empty,
    Padding,
}

#[derive(Clone, Copy, PartialEq, Debug, Eq)]
pub enum MoveType {
    Default,
    Capture,
    CastleKingside,
    CastleQueenside,
}

impl Square {
    fn unicode_str(&self) -> Option<&str> {
        match self {
            Self::Full(p) => Some(p.unicode_str()),
            Self::Empty => Some(" ⠀"),
            Self::Padding => None,
        }
    }

    const fn color(&self) -> Option<PieceColor> {
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
    pub rank: i8,
    pub file: i8,
}

impl Field {
    pub const fn new(rank: i8, file: i8) -> Self {
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

#[derive(Debug)]
///Used to store which castling rights have been lost by this move
pub struct ChangedCastlingRights {
    white_king: bool,
    white_queen: bool,
    black_king: bool,
    black_queen: bool,
}

impl ChangedCastlingRights {
    fn new(old: &CastlingRights, new: &CastlingRights) -> Self {
        //println!("old castling rights: {:?}", old);
        //println!("new castling rights: {:?}", new);
        Self {
            white_king: old.white_king_castle_lost_ply.is_none()
                != new.white_king_castle_lost_ply.is_none(),
            white_queen: old.white_queen_castle_lost_ply.is_none()
                != new.white_queen_castle_lost_ply.is_none(),
            black_king: old.black_king_castle_lost_ply.is_none()
                != new.black_king_castle_lost_ply.is_none(),
            black_queen: old.black_queen_castle_lost_ply.is_none()
                != new.black_queen_castle_lost_ply.is_none(),
        }
    }
}

#[derive(Clone, Copy, PartialOrd, Debug, PartialEq, Eq)]
pub enum PieceColor {
    Black,
    White,
}

impl PieceColor {
    pub const fn opposite(&self) -> Self {
        match *self {
            Self::Black => Self::White,
            Self::White => Self::Black,
        }
    }
}

impl Display for PieceColor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    #[error("piece on {0} has wrong color")]
    WrongColor(Field),
    #[error("cannot move to {0}")]
    BadMoveTarget(Field),
    #[error("cannot parse symbol")]
    Parse,
}

#[cfg(test)]
mod tests {
    use crate::evaluation::evaluate_rel;

    use super::*;

    #[test]
    fn rook_check() {
        let b = BoardState::from_fen("3r4/2k3R1/p7/8/8/8/6K1/8 b").unwrap();
        assert!(b.check(PieceColor::Black));
    }

    #[test]
    fn knight_check() {
        let b = BoardState::from_fen("3r4/2k5/p5R1/8/8/4n3/6K1/8 w").unwrap();
        assert!(b.check(PieceColor::White));
    }

    #[test]
    fn knight_no_check() {
        let b = BoardState::from_fen("3r4/2k5/p5R1/8/8/8/4n1K1/8 w").unwrap();
        assert!(!b.check(PieceColor::White));
    }

    #[test]
    fn bishop_check() {
        let b = BoardState::from_fen("3r4/2k5/p5R1/8/5B2/8/4n1K1/8 w").unwrap();
        assert!(b.check(PieceColor::Black));
    }

    #[test]
    fn pawn_check() {
        let b = BoardState::from_fen("3r4/2k5/p5R1/8/8/4B2p/4n1K1/8 w").unwrap();
        assert!(b.check(PieceColor::White));
    }

    #[test]
    fn queen_check() {
        let b = BoardState::from_fen("3r4/2k3Q1/p5R1/8/8/p3B3/4n1K1/8 b").unwrap();
        assert!(b.check(PieceColor::Black));
    }
    /*
    #[test]
    fn back_rank_mate() {
        let mut b = BoardState::from_fen("1k4Q1/ppp5/8/8/8/8/6K1/8 b").unwrap();
        assert!(b.check(PieceColor::Black));
        assert_eq!(evaluate_rel(&mut b, false), -i32::MAX);
    }*/

    #[test]
    fn mate_in_1_depth_1() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let (_, m) = b.iterative_deepening_nega(None, Some(1));
        b.make(&m.unwrap());
        assert!(b.check(PieceColor::Black));
        assert_eq!(evaluate_rel(&mut b, false), -i32::MAX);
    }

    #[test]
    fn mate_in_1_depth_2() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let (_, m) = b.iterative_deepening_nega(None, Some(2));
        b.make(&m.unwrap());
        assert!(b.check(PieceColor::Black));
        assert_eq!(evaluate_rel(&mut b, false), -i32::MAX);
    }

    //Achtung falls verhalten sich ändert mit hashtable, weil hier castling rights in der start-
    //position ignoriert werden
    #[test]
    fn mate_in_1_depth_3() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let (_, m) = b.iterative_deepening_nega(None, Some(3));
        b.make(&m.unwrap());
        assert!(b.check(PieceColor::Black));
        assert_eq!(evaluate_rel(&mut b, false), -i32::MAX);
    }
    /*
    #[test]
    fn mate_in_3_endgame() {
        let b = BoardState::from_fen("7R/2N1P3/8/8/8/8/k6K/8 w").unwrap();
        let (_, m) = b.iterative_deepening_nega(None, Some(5));
        assert_eq!(m.unwrap().to, Field::new(8, 2));
    }*/

    #[test]
    #[should_panic]
    fn castling_rights_in_fen_but_impossible() {
        BoardState::from_fen("rnbqkbnr/pp1p3p/2p3p1/4pp2/8/4PN2/PPPPBPPP/RNBQ1RK1 w KQkq").unwrap();
    }

    #[test]
    fn white_pawn_take_left() {
        let b = BoardState::from_fen("rnbqkbnr/pppp1ppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(4, 5), false).unwrap(),
            vec![Field::new(5, 4), Field::new(5, 5)],
        );
    }

    #[test]
    fn white_pawn_not_take_center() {
        let b = BoardState::from_fen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(4, 5), false).unwrap(),
            vec![],
        );
    }

    #[test]
    fn white_pawn_take_right() {
        let b = BoardState::from_fen("rnbqkbnr/pppp1ppp/8/5p2/4P3/8/PPPP1PPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(4, 5), false).unwrap(),
            vec![Field::new(5, 6), Field::new(5, 5)],
        );
    }

    #[test]
    fn black_pawn_take_left() {
        let b = BoardState::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR b").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(5, 5), false).unwrap(),
            vec![Field::new(4, 4), Field::new(4, 5)],
        );
    }

    #[test]
    fn black_pawn_not_take_center() {
        let b = BoardState::from_fen("rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR b").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(5, 4), false).unwrap(),
            vec![],
        );
    }

    #[test]
    fn black_pawn_take_right() {
        let b = BoardState::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR b").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(5, 4), false).unwrap(),
            vec![Field::new(4, 4), Field::new(4, 5)],
        );
    }

    #[test]
    fn white_pawn_double_push() {
        let b = BoardState::new();
        eq_fields(
            b.pseudo_legal_moves(Field::new(2, 1), false).unwrap(),
            vec![Field::new(3, 1), Field::new(4, 1)],
        );
    }

    #[test]
    fn black_pawn_double_push() {
        let b = BoardState::new();
        eq_fields(
            b.pseudo_legal_moves(Field::new(7, 1), false).unwrap(),
            vec![Field::new(6, 1), Field::new(5, 1)],
        );
    }

    #[test]
    fn white_pawn_double_push_blocked() {
        let b =
            BoardState::from_fen("rnbqkbnr/1ppppppp/8/8/p7/8/PPPPPPPP/RNBQKBNR w KQkq").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(2, 1), false).unwrap(),
            vec![Field::new(3, 1)],
        );
    }

    #[test]
    fn black_pawn_double_push_blocked() {
        let b = BoardState::from_fen("rnbqkbnr/pppppppp/8/P7/8/8/1PPPPPPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(7, 1), false).unwrap(),
            vec![Field::new(6, 1)],
        );
    }

    #[test]
    fn no_en_passant() {
        let b = BoardState::from_fen("7k/8/8/4pP2/8/8/1K6/8 w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(5, 6), false).unwrap(),
            vec![Field::new(6, 6)],
        );
    }

    #[test]
    fn en_passant_in_fen() {
        let b = BoardState::from_fen("7k/8/8/4pP2/8/8/1K6/8 w - e6").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(5, 6), false).unwrap(),
            vec![Field::new(6, 5), Field::new(6, 6)],
        );
    }

    #[test]
    fn en_passant_in_fen_black() {
        let b = BoardState::from_fen("7k/8/8/8/3pP3/K7/8/8 b - e3").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(4, 4), false).unwrap(),
            vec![Field::new(3, 4), Field::new(3, 5)],
        );
    }

    #[test]
    fn en_passant_after_double_push() {
        let mut b = BoardState::from_fen("7k/8/8/8/3p4/K7/4P3/8 w - -").unwrap();
        assert_eq!(b.en_passant(), None);
        b.move_by_str("e2e4");
        assert_eq!(b.en_passant(), Some(Field::new(3, 5)));
        b.move_by_str("d4e3");
        assert_eq!(b.en_passant(), None);
    }

    #[test]
    fn white_pawn_not_take_white_pawn() {
        let b =
            BoardState::from_fen("rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(2, 4), false).unwrap(),
            vec![Field::new(3, 4), Field::new(4, 4)],
        );
    }

    #[test]
    fn white_knight_takes() {
        let b = BoardState::from_fen("rnbqkbnr/pp1ppppp/8/8/8/2p5/PPPPPPPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(1, 2), false).unwrap(),
            vec![Field::new(3, 1), Field::new(3, 3)],
        );
    }

    #[test]
    fn black_knight_corner_blocked() {
        let b = BoardState::from_fen("n7/2p3k1/1p6/8/8/4K3/8/8 w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(8, 1), false).unwrap(),
            vec![],
        );
    }

    #[test]
    fn black_knight_corner_takes() {
        let b = BoardState::from_fen("n7/2p3k1/1P6/8/8/4K3/8/8 w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(8, 1), false).unwrap(),
            vec![Field::new(6, 2)],
        );
    }

    #[test]
    fn black_knight_edge_takes() {
        let b = BoardState::from_fen("8/1k3p2/7n/8/6P1/8/2K5/8 w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(6, 8), false).unwrap(),
            vec![Field::new(8, 7), Field::new(5, 6), Field::new(4, 7)],
        );
    }

    #[test]
    fn black_knight_edge_blocked() {
        let b = BoardState::from_fen("6p1/1k3p2/7n/5p2/6p1/8/1K6/8 w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(6, 8), false).unwrap(),
            vec![],
        );
    }

    #[test]
    fn white_bishop_attack_partially_blocked() {
        let b = BoardState::from_fen("8/8/8/4R3/1p6/2B4k/8/p6K w").unwrap();
        eq_fields(
            b.pseudo_legal_moves(Field::new(3, 3), false).unwrap(),
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

    #[test]
    fn correct_hash_in_board_state() {
        let b1 = BoardState::new();
        let hasher = ZobristState::from_board_state(&b1);
        assert_eq!(hasher.hash, b1.zobrist.hash);
    }

    #[test]
    fn hash_from_board_state() {
        let b1 =
            BoardState::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq").unwrap();
        let correct_hash = zobrist::ZobristState::from_board_state(&b1).hash;

        //hash von initial position nehmen, change_hahs aufrufen für die pieces
        //soll nur schauen, dass der hash mal deterministisch ist.

        let b2 = BoardState::new();
        let mut test_hasher = zobrist::ZobristState::from_board_state(&b2);
        test_hasher.change_piece(
            Field::new(2, 5),
            Piece {
                kind: PieceKind::Pawn,
                color: PieceColor::White,
            },
        );
        test_hasher.change_piece(
            Field::new(4, 5),
            Piece {
                kind: PieceKind::Pawn,
                color: PieceColor::White,
            },
        );
        test_hasher.change_black_to_move();
        test_hasher.change_piece(
            Field::new(7, 4),
            Piece {
                kind: PieceKind::Pawn,
                color: PieceColor::Black,
            },
        );
        test_hasher.change_piece(
            Field::new(5, 4),
            Piece {
                kind: PieceKind::Pawn,
                color: PieceColor::Black,
            },
        );
        test_hasher.change_black_to_move();
        assert_eq!(correct_hash, test_hasher.hash);
    }

    #[test]
    fn hash_castling_rights() {
        let b1 = BoardState::from_fen("rnbqkb1r/p1p1pppp/1p3n2/3p4/4P3/5N2/PPPPBPPP/RNBQ1RK1 b kq")
            .unwrap();
        let correct_hash = zobrist::ZobristState::from_board_state(&b1).hash;

        //hash von initial position nehmen, change_hahs aufrufen für die pieces
        //soll nur schauen, dass der hash deterministisch ist.

        let b2 =
            BoardState::from_fen("rnbqkb1r/p1p1pppp/1p3n2/3p4/4P3/5N2/PPPPBPPP/RNBQK2R w KQkq")
                .unwrap();
        let mut test_hasher = zobrist::ZobristState::from_board_state(&b2);
        //Move king
        test_hasher.change_piece(
            Field::new(1, 5),
            Piece {
                kind: PieceKind::King,
                color: PieceColor::White,
            },
        );
        test_hasher.change_piece(
            Field::new(1, 7),
            Piece {
                kind: PieceKind::King,
                color: PieceColor::White,
            },
        );
        //Move rook
        test_hasher.change_piece(
            Field::new(1, 8),
            Piece {
                kind: PieceKind::Rook,
                color: PieceColor::White,
            },
        );
        test_hasher.change_piece(
            Field::new(1, 6),
            Piece {
                kind: PieceKind::Rook,
                color: PieceColor::White,
            },
        );
        test_hasher.change_castling_rights(MoveType::CastleKingside, PieceColor::White);
        test_hasher.change_castling_rights(MoveType::CastleQueenside, PieceColor::White);
        test_hasher.change_black_to_move();
        assert_eq!(correct_hash, test_hasher.hash);
    }

    ///Compare v1 and v2, ignoring the order of the fields
    fn eq_fields(v1: Vec<Move>, mut v2: Vec<Field>) {
        let mut fields: Vec<Field> = v1.into_iter().map(|m| m.to).collect();
        fields.sort_unstable();
        v2.sort_unstable();
        assert_eq!(fields, v2);
    }
}
