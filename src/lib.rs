//#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
mod evaluation;
mod zobrist;

use std::fmt::{self, Display};

use colored::Colorize;
use ndarray::prelude::*;

const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq";

pub struct BoardState {
    pub board: Board,
    pub turn: PieceColor,
    ply: u16,
    pub castling_rights: CastlingRights,

    //en_passant: Option<Field>,
    moves: Vec<Move>,
    taken: Vec<Piece>,
}

impl BoardState {
    pub fn new() -> Self {
        Self::from_fen(STARTING_POSITION).unwrap()
    }

    ///Static evaluation
    ///
    /// Positive: White's advantage
    pub fn evaluate(&mut self) -> i32 {
        evaluation::evaluate(self)
    }

    ///Assumes root has color state.turn
    pub fn minimax(&mut self, depth: i32) -> (Option<Move>, i32) {
        // println!("Enter minimax, depth {}", depth);
        //todo!("crasht sobald es ein forced checkmate sieht. returnt normal, best_move wird nie gesetzt. #moves > 0");
        let mut best_move = None;
        //alpha is the most positive eval that white is already assured of at this point
        //beta is the most negative eval that black is already assured of at this point
        //Root cannot cut branches! (Otherwise, what would be the point of having a tree?)
        let mut alpha = i32::MIN;
        let mut beta = i32::MAX;
        let mut moves = 0;
        match self.turn {
            //White is the maximizing player
            PieceColor::White => {
                let mut best_value = i32::MIN;
                for m in self.generate_moves() {
                    // println!("root: white try move {}", m);

                    if alpha == i32::MAX || beta == i32::MIN {
                        println!("forced checkmate, early return.");
                        return (best_move, best_value);
                    }

                    self.make(m);
                    // self.draw_board(true);

                    //It's now black's turn
                    //If this move didn't put white into check, recurse.
                    //If it did put white into check, ignore.
                    if !self.check(self.turn.opposite()) {
                        //In case of a forced checkmate sequence, just play the first
                        //LEGAL move (legal = does not put me in check)
                        best_move.get_or_insert(m);
                        // println!("root: didnt put white in check.");
                        let score = self.minimax_helper(depth - 1, alpha, beta);
                        moves += 1;
                        if score > best_value {
                            best_move = Some(m);
                            best_value = score;
                            // println!("root: best value updated to: {}", best_value);
                        }
                        if score > alpha {
                            alpha = score;
                            // println!("root: alpha updated to: {}", alpha);
                        }
                    } else {
                        // println!("root: but put white in check");
                    }
                    self.unmake();
                }

                //White has no legal moves in the original position
                if moves == 0 {
                    if self.checkmate_given_zero_moves(PieceColor::Black) {
                        println!("CHECKMATE! BLACK WINS!");
                        return (None, i32::MIN);
                    } else {
                        println!("STALEMATE! THE GAME IS A DRAW!");
                        return (None, 0);
                    }
                }
                println!("Found {} legal moves", moves);
                (best_move, best_value)
            }

            //Black is the minimizing player
            PieceColor::Black => {
                let mut best_value = i32::MAX;
                for m in self.generate_moves() {
                    // println!("root: black try move {}", m);
                    if alpha == i32::MAX || beta == i32::MIN {
                        println!("forced checkmate, early return.");
                        return (best_move, best_value);
                    }
                    self.make(m);
                    // self.draw_board(true);
                    //It's now black's turn
                    //If this move didn't put white into check, recurse
                    if !self.check(self.turn.opposite()) {
                        best_move.get_or_insert(m);
                        // println!("root: didnt put black in check");
                        let score = self.minimax_helper(depth - 1, alpha, beta);
                        moves += 1;
                        if score < best_value {
                            best_move = Some(m);
                            best_value = score;
                            // println!("root: best value updated to: {}", best_value);
                        }
                        if score < beta {
                            beta = score;
                            // println!("root: beta updated to: {}", beta);
                        }
                    } else {
                        // println!("root: but put black in check");
                    }
                    self.unmake();
                }
                if moves == 0 {
                    if self.checkmate_given_zero_moves(PieceColor::White) {
                        println!("CHECKMATE! WHITE WINS!");
                        return (None, i32::MAX);
                    } else {
                        println!("STALEMATE! THE GAME IS A DRAW!");
                        return (None, 0);
                    }
                }

                println!("Found {} legal moves", moves);
                (best_move, best_value)
            }
        }
    }

    fn minimax_helper(&mut self, depth: i32, mut alpha: i32, mut beta: i32) -> i32 {
        //alpha is the most positive eval that white is already assured of at this point
        //beta is the most negative eval that black is already assured of at this point
        if depth == 0 {
            let res = self.evaluate();
            // println!("call evaluate() returned {}", res);
            res
        } else {
            let mut moves = 0;
            match self.turn {
                //White is the maximizing player
                PieceColor::White => {
                    let mut best_value = i32::MIN;
                    for m in self.generate_moves() {
                        // println!("{}: white try move {}", depth, m);
                        //Branch cut: Don't explore this subtree further if it is already
                        //obvious that this variant will not be taken.
                        if beta <= alpha {
                            // println!("beta <= alpha: return");
                            return best_value;
                        }
                        self.make(m);
                        // self.draw_board(true);
                        //It is now black's turn
                        //If this move didn't put white into check, recurse
                        if !self.check(self.turn.opposite()) {
                            // println!("{}: didnt put white in check", depth);
                            let score = self.minimax_helper(depth - 1, alpha, beta);
                            moves += 1;
                            if score > best_value {
                                best_value = score;
                                // println!("{}: best value updated to: {}", depth, best_value);
                            }
                            if score > alpha {
                                alpha = score;
                                // println!("{}: alpha updated to: {}", depth, alpha);
                            }
                        } else {
                            // println!("{}: but put white in check", depth);
                        }
                        self.unmake();
                    }
                    if moves == 0 && !self.checkmate_given_zero_moves(PieceColor::Black) {
                        //This position is stalemate
                        best_value = 0;
                        // println!("{}: no moves but no checkmate", depth);
                    }
                    best_value
                }
                //Black is the minimizing player
                PieceColor::Black => {
                    let mut best_value = i32::MAX;
                    for m in self.generate_moves() {
                        // println!("{}: black try move {}", depth, m);
                        if beta <= alpha {
                            // println!("beta <= alpha: return");
                            return best_value;
                        }
                        self.make(m);
                        // self.draw_board(true);
                        //It's now black's turn
                        //If this move didn't put white into check, recurse
                        if !self.check(self.turn.opposite()) {
                            // println!("{}: didnt put black in check", depth);
                            let score = self.minimax_helper(depth - 1, alpha, beta);
                            moves += 1;
                            if score < best_value {
                                best_value = score;
                                // println!("{}: best value updated to: {}", depth, best_value);
                            }
                            if score < beta {
                                beta = score;
                                // println!("{}: beta updated to: {}", depth, beta);
                            }
                        } else {
                            // println!("{}: but put black in check", depth);
                        }
                        self.unmake();
                    }
                    if moves == 0 && !self.checkmate_given_zero_moves(PieceColor::White) {
                        //This position is stalemate
                        best_value = 0;
                        // println!("{}: no moves but no checkmate", depth);
                    }
                    best_value
                }
            }
        }
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

    ///Move piece by string like "e2e4". Checks for pseudo-legality.
    pub fn move_by_str(&mut self, arg: &str) -> Result<(), Error> {
        println!("\nTry to process input: {}", arg);

        let mut chars = arg.chars();

        let from_file = Board::file_letter_to_number(chars.next().ok_or(Error::Parse)?)? as i8;
        let from_rank = chars
            .next()
            .ok_or(Error::Parse)?
            .to_digit(10)
            .ok_or(Error::Parse)? as i8;

        if let Square::Full(p) = self.board.at(from_rank, from_file) {
            if p.color != self.turn {
                eprintln!("{} to move!", self.turn.to_string().red());
                return Err(Error::WrongColor(Field::new(from_rank, from_file)));
            }
        } else {
            return Err(Error::NoPieceOnField(Field::new(from_rank, from_file)));
        }

        //From here on we can be sure that it's the correct side to move and the square is
        //actually full.

        let to_file = Board::file_letter_to_number(chars.next().ok_or(Error::Parse)?)? as i8;
        let to_rank = chars
            .next()
            .ok_or(Error::Parse)?
            .to_digit(10)
            .ok_or(Error::Parse)? as i8;

        let from = Field::new(from_rank, from_file);
        let to = Field::new(to_rank, to_file);

        let mut v = self
            .board
            .pseudo_legal_moves(from)
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
            self.make(m);
            if self.check(self.turn.opposite()) {
                println!("You're in check!");
                self.unmake();
                return Err(Error::BadMoveTarget(Field::new(to_rank, to_file)));
            }
            Ok(())
        } else {
            Err(Error::BadMoveTarget(Field::new(to_rank, to_file)))
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

    pub fn generate_moves(&self) -> Vec<Move> {
        //Going through all fields, checking whether there is a piece, then calling
        //pseudo_legal_moves feels quite inefficient...

        //Eventually I want to keep a list of currently alive pieces. Then only go through
        //those //todo!
        let mut v: Vec<Move> = self.castling_moves();
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
        v = self.board.avoid_king_captures(v);
        v
    }

    ///Assumes the move is fully legal!
    pub fn make(&mut self, m: Move) {
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
                MoveType::CastleKingside => {
                    //King's position is updated below (independent of whether it's castling
                    //or a normal move.)
                    if self.turn == PieceColor::White {
                        let rook = self.board.at(1, 8);
                        self.board.set(1, 8, Square::Empty);
                        self.board.set(1, 6, rook);
                    } else {
                        let rook = self.board.at(8, 8);
                        self.board.set(8, 8, Square::Empty);
                        self.board.set(8, 6, rook);
                    }
                }
                MoveType::CastleQueenside => {
                    if self.turn == PieceColor::White {
                        let rook = self.board.at(1, 1);
                        self.board.set(1, 1, Square::Empty);
                        self.board.set(1, 4, rook);
                    } else {
                        let rook = self.board.at(8, 1);
                        self.board.set(8, 1, Square::Empty);
                        self.board.set(8, 4, rook);
                    }
                }
            }

            //King moves must be handled specifically. Update the state variable.

            //Since castling is a move from the king's position, moving_piece.kind==King,
            //so both castling rights are removed and the king's position is updated.

            match moving_piece.kind {
                PieceKind::King => {
                    if moving_piece.color == PieceColor::White {
                        self.board.white_king = Field::new(m.to.rank, m.to.file);
                        self.castling_rights
                            .white_king_castle_lost_ply
                            .get_or_insert(self.ply);
                        self.castling_rights
                            .white_queen_castle_lost_ply
                            .get_or_insert(self.ply);
                    } else {
                        self.board.black_king = Field::new(m.to.rank, m.to.file);
                        self.castling_rights
                            .black_king_castle_lost_ply
                            .get_or_insert(self.ply);
                        self.castling_rights
                            .black_queen_castle_lost_ply
                            .get_or_insert(self.ply);
                    }
                }
                PieceKind::Rook => {
                    //We can skip checking the color because the move is assumed to be
                    //legal, so if it is from a1, it must be white's move.
                    if m.from == Field::new(1, 1) {
                        //white queen-side
                        self.castling_rights
                            .white_queen_castle_lost_ply
                            .get_or_insert(self.ply);
                    } else if m.from == Field::new(1, 8) {
                        //white king-side
                        self.castling_rights
                            .white_king_castle_lost_ply
                            .get_or_insert(self.ply);
                    } else if m.from == Field::new(8, 1) {
                        //black queen-side
                        self.castling_rights
                            .black_queen_castle_lost_ply
                            .get_or_insert(self.ply);
                    } else if m.from == Field::new(8, 8) {
                        //black king-side
                        self.castling_rights
                            .black_king_castle_lost_ply
                            .get_or_insert(self.ply);
                    }
                }
                _ => (),
            }

            //The from square is now empty.

            self.board.set(m.from.rank, m.from.file, Square::Empty);

            //Promote if it is a promotion move, otherwise move the original piece
            self.board.set(
                m.to.rank,
                m.to.file,
                Square::Full(m.promotion.unwrap_or(moving_piece)),
            );
        }

        self.turn = self.turn.opposite();
        self.ply += 1;
        //println!("make end {}", m);
        //Add the moves to self.moves, and if attacking move, add the taken piece to taken.
        //Add castle and promotion to movetype (they are all mutually exclusive).
        //If castle, restore or take away castling right for whose turn it is.
    }

    ///Unmakes the last move (the one at the end of the moves vec)
    pub fn unmake(&mut self) {
        //println!("unmake start");
        self.turn = self.turn.opposite();
        self.ply -= 1;

        if let Some(m) = self.moves.pop() {
            //to and from are from the perspective of the original move.

            if let Square::Full(moving_piece) = self.board.at(m.to.rank, m.to.file) {
                self.board.set(m.to.rank, m.to.file, Square::Empty);

                //If it is a promoting move, the piece must have been a pawn before!
                if m.promotion.is_some() {
                    self.board.set(
                        m.from.rank,
                        m.from.file,
                        Square::Full(Piece::pawn(self.turn)),
                    );
                } else {
                    self.board
                        .set(m.from.rank, m.from.file, Square::Full(moving_piece));
                }

                //Handle captures and castling (update rook position)

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
                    MoveType::CastleKingside => {
                        if self.turn == PieceColor::White {
                            let rook = self.board.at(1, 6);
                            self.board.set(1, 6, Square::Empty);
                            self.board.set(1, 8, rook);
                        } else {
                            let rook = self.board.at(8, 6);
                            self.board.set(8, 6, Square::Empty);
                            self.board.set(8, 8, rook);
                        }
                    }
                    MoveType::CastleQueenside => {
                        if self.turn == PieceColor::White {
                            let rook = self.board.at(1, 4);
                            self.board.set(1, 4, Square::Empty);
                            self.board.set(1, 1, rook);
                        } else {
                            let rook = self.board.at(8, 4);
                            self.board.set(8, 4, Square::Empty);
                            self.board.set(8, 1, rook);
                        }
                    }
                }

                //Update king position

                if matches!(moving_piece.kind, PieceKind::King) {
                    if moving_piece.color == PieceColor::White {
                        self.board.white_king = Field::new(m.from.rank, m.from.file);
                    } else {
                        self.board.black_king = Field::new(m.from.rank, m.from.file);
                    }
                }

                //Restore the castling rights which apply
                if let Some(castle_ply) = self.castling_rights.white_king_castle_lost_ply {
                    if self.ply == castle_ply {
                        self.castling_rights.white_king_castle_lost_ply = None;
                    }
                }
                if let Some(castle_ply) = self.castling_rights.white_queen_castle_lost_ply {
                    if self.ply == castle_ply {
                        self.castling_rights.white_queen_castle_lost_ply = None;
                    }
                }
                if let Some(castle_ply) = self.castling_rights.black_king_castle_lost_ply {
                    if self.ply == castle_ply {
                        self.castling_rights.black_king_castle_lost_ply = None;
                    }
                }
                if let Some(castle_ply) = self.castling_rights.black_queen_castle_lost_ply {
                    if self.ply == castle_ply {
                        self.castling_rights.black_queen_castle_lost_ply = None;
                    }
                }
            } else {
                self.draw(true);
                panic!("No piece at destination in unmake");
            }
        } else {
            panic!("Trying to unmake but there is no previous move");
        }
        //println!("unmake end");
        //Revert the above by popping from the stack
    }

    fn threatened(&self, victim_color: PieceColor, field: Field) -> bool {
        //Kinght Moves

        for m in self.board.pseudo_legal_knight_moves(victim_color, field) {
            let Field { rank, file } = m.to;
            if let Square::Full(p) = self.board.at(rank, file) {
                if matches!(p.kind, PieceKind::Knight) {
                    return true;
                }
            }
        }

        //Rooks/Queens

        for m in self.board.pseudo_legal_rook_moves(victim_color, field) {
            let Field { rank, file } = m.to;
            if let Square::Full(p) = self.board.at(rank, file) {
                if matches!(p.kind, PieceKind::Rook) || matches!(p.kind, PieceKind::Queen) {
                    return true;
                }
            }
        }

        //Bishops/Queens

        for m in self.board.pseudo_legal_bishop_moves(victim_color, field) {
            let Field { rank, file } = m.to;
            if let Square::Full(p) = self.board.at(rank, file) {
                if matches!(p.kind, PieceKind::Bishop) || matches!(p.kind, PieceKind::Queen) {
                    return true;
                }
            }
        }

        //Pawns

        for m in self.board.pseudo_legal_pawn_moves(victim_color, field) {
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

    ///Returns all valid castling moves of player whose turn it currently is.
    fn castling_moves(&self) -> Vec<Move> {
        let mut v = vec![];
        //Check cheap conditions (empty, castling rights) first, then expensive (threatened)

        if self.turn == PieceColor::White {
            if self.castling_rights.white_king_castle_lost_ply.is_none()
                && self.board.empty(1, 6)
                && self.board.empty(1, 7)
                && !self.check(self.turn)
                && !self.threatened(self.turn, Field::new(1, 6))
                && !self.threatened(self.turn, Field::new(1, 7))
            {
                v.push(Move::new(
                    Field::new(1, 5),
                    Field::new(1, 7),
                    MoveType::CastleKingside,
                ));
            }
            if self.castling_rights.white_queen_castle_lost_ply.is_none()
                && self.board.empty(1, 4)
                && self.board.empty(1, 3)
                && self.board.empty(1, 2)
                && !self.check(self.turn)
                && !self.threatened(self.turn, Field::new(1, 4))
                && !self.threatened(self.turn, Field::new(1, 3))
            {
                v.push(Move::new(
                    Field::new(1, 5),
                    Field::new(1, 3),
                    MoveType::CastleQueenside,
                ));
            }
        } else {
            //Black's turn
            if self.castling_rights.black_king_castle_lost_ply.is_none()
                && self.board.empty(8, 6)
                && self.board.empty(8, 7)
                && !self.check(self.turn)
                && !self.threatened(self.turn, Field::new(8, 6))
                && !self.threatened(self.turn, Field::new(8, 7))
            {
                v.push(Move::new(
                    Field::new(8, 5),
                    Field::new(8, 7),
                    MoveType::CastleKingside,
                ));
            }
            if self.castling_rights.black_queen_castle_lost_ply.is_none()
                && self.board.empty(8, 4)
                && self.board.empty(8, 3)
                && self.board.empty(8, 2)
                && !self.check(self.turn)
                && !self.threatened(self.turn, Field::new(8, 4))
                && !self.threatened(self.turn, Field::new(8, 3))
            {
                v.push(Move::new(
                    Field::new(8, 5),
                    Field::new(8, 3),
                    MoveType::CastleQueenside,
                ));
            }
        }
        v
    }

    ///Draw chess board using unicode characters.
    /// todo: could draw pseudolegal moves for a specific piece (maybe in
    /// another function)
    pub fn draw_board(&self, show_prev: bool) {
        for rank in (1..9).rev() {
            print!("{}", rank);
            for file in 1..9 {
                let square = self.board.at(rank, file);
                let field = Field::new(rank, file);
                let tile = match square.unicode_str() {
                    Some(s) => s,
                    None => continue,
                };

                let mut tile = if (rank + file) % 2 == 0 {
                    tile.on_truecolor(158, 93, 30) //black
                } else {
                    tile.on_truecolor(155, 120, 70) //white
                };

                if show_prev {
                    if let Some(&last) = self.moves.last() {
                        if last.from == field || last.to == field {
                            tile = tile.on_truecolor(122, 156, 70);
                        }
                    }
                }

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

    ///Consumes one trailing whitespace, if any.
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
            if rank < 1 || rank > 8 || file < 1 || file > 8 {
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

    ///Leave trailing whitespaces as they are
    fn parse_turn_fen(chars: &mut std::str::Chars) -> Result<PieceColor, Error> {
        let mut turn = PieceColor::White;

        if let Some(c) = chars.by_ref().next() {
            turn = match c {
                'w' | 'W' => PieceColor::White,
                'b' | 'B' => PieceColor::Black,
                _ => return Err(Error::Parse),
            }
        };
        Ok(turn)
    }

    fn from_fen(fen: &str) -> Result<BoardState, Error> {
        let chars = &mut fen.chars();

        let board = Self::parse_board_fen(chars)?;

        let turn = Self::parse_turn_fen(chars)?;

        chars.by_ref().next(); //Consume the whitespace if it's there

        let castling_rights = parse_castling_rights_fen(chars, &board)?;

        Ok(Self {
            board,
            turn,
            ply: 1,
            castling_rights,
            moves: vec![],
            taken: vec![],
        })
    }
}

///Consumes trailing whitespace, if any.
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
#[derive(Copy, Clone)]
pub struct Move {
    pub from: Field,
    pub to: Field,
    move_type: MoveType,
    promotion: Option<Piece>,
}

impl Move {
    const fn new(from: Field, to: Field, move_type: MoveType) -> Self {
        Move {
            from,
            to,
            move_type,
            promotion: None,
        }
    }

    ///Generate a new move which may be a promotion (or not).
    const fn promotion(from: Field, to: Field, move_type: MoveType, promo: Option<Piece>) -> Self {
        Move {
            from,
            to,
            move_type,
            promotion: promo,
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

    ///May return NoPieceOnField Error
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
        let starting_rank;
        let promotion_rank;
        let move_offset;
        let opposite_color = color.opposite();

        match color {
            PieceColor::White => {
                starting_rank = 2;
                promotion_rank = 7;
                move_offset = 1;
            }
            PieceColor::Black => {
                starting_rank = 7;
                promotion_rank = 2;
                move_offset = -1;
            }
        }

        let mut moves = match self.at(rank + move_offset, file) {
            //If one square in move direction is empty
            Square::Empty => {
                let mut standard_moves = vec![];

                standard_moves.push(Move::promotion(
                    from,
                    Field::new(rank + move_offset, file),
                    MoveType::Default,
                    if rank == promotion_rank {
                        Some(Piece::queen(color))
                    } else {
                        None
                    },
                ));

                //Can never be a promotion move
                if rank == starting_rank {
                    //Double push
                    if let Square::Empty = self.at(rank + 2 * move_offset, file) {
                        standard_moves.push(Move::new(
                            from,
                            Field::new(rank + 2 * move_offset, file),
                            MoveType::Default,
                        ));
                    }
                }
                standard_moves
            }
            _ => vec![],
        };
        //Attack to the left
        //Remember this could also be a promotion move.
        match self.at(rank + move_offset, file - 1) {
            Square::Full(p) if p.color == opposite_color => {
                moves.push(Move::promotion(
                    from,
                    Field::new(rank + move_offset, file - 1),
                    MoveType::Capture,
                    if rank == promotion_rank {
                        Some(Piece::queen(color))
                    } else {
                        None
                    },
                ));
            }
            _ => (),
        }

        //Attack to the right
        match self.at(rank + move_offset, file + 1) {
            Square::Full(p) if p.color == opposite_color => {
                moves.push(Move::promotion(
                    from,
                    Field::new(rank + move_offset, file + 1),
                    MoveType::Capture,
                    if rank == promotion_rank {
                        Some(Piece::queen(color))
                    } else {
                        None
                    },
                ));
            }
            _ => (),
        }
        moves
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

    fn pseudo_legal_king_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let Field { rank, file } = from;
        let v = vec![
            Field::new(rank + 1, file - 1),
            Field::new(rank + 1, file),
            Field::new(rank + 1, file + 1),
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
        for i in (1..rank).rev() {
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
        for j in (1..file).rev() {
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
            .into_iter()
            .filter(|&to| {
                if let Square::Full(p) = self.at(to.rank, to.file) {
                    p.color == opposite
                } else {
                    false
                }
            })
            .map(|to| Move::new(from, to, MoveType::Capture))
            .collect();

        default_moves.extend(captures);
        default_moves
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

    fn avoid_king_captures(&self, v: Vec<Move>) -> Vec<Move> {
        v.into_iter()
            .filter(|m| m.to != self.white_king && m.to != self.black_king)
            .collect()
    }

    ///Returns true if piece is on that field.
    fn piece_at(&self, field: Field, piece: Piece) -> bool {
        if let Square::Full(p) = self.at(field.rank, field.file) {
            return p == piece;
        }
        false
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum PieceKind {
    Pawn,
    Bishop,
    Knight,
    Rook,
    Queen,
    King,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
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

    const fn unicode_str(&self) -> &str {
        match self.kind {
            PieceKind::Pawn => "♟︎ ",
            PieceKind::Bishop => "♝ ",
            PieceKind::Knight => "♞ ",
            PieceKind::Rook => "♜ ",
            PieceKind::Queen => "♛ ",
            PieceKind::King => "♚ ",
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
    CastleKingside,
    CastleQueenside,
}

impl Square {
    const fn unicode_str(&self) -> Option<&str> {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    #[error("piece on {0} has wrong color")]
    WrongColor(Field),
    #[error("cannot move to {0}")]
    BadMoveTarget(Field),
    #[error("cannot parse symbol")]
    Parse,
}

#[cfg(test)]
mod tests {
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

    #[test]
    fn back_rank_mate() {
        let mut b = BoardState::from_fen("1k4Q1/ppp5/8/8/8/8/6K1/8 b").unwrap();
        assert!(b.check(PieceColor::Black));
        assert_eq!(b.evaluate(), i32::MAX);
    }

    #[test]
    fn mate_in_1_depth_1() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let m = b.minimax(1);
        assert!(m.1 == i32::MAX);
        b.make(m.0.unwrap());
        assert!(b.check(PieceColor::Black));
        assert_eq!(b.evaluate(), i32::MAX);
    }

    #[test]
    fn mate_in_1_depth_2() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let m = b.minimax(2);
        println!("got {}, {}", m.0.unwrap(), m.1);
        assert!(m.1 == i32::MAX);
        b.make(m.0.unwrap());
        assert!(b.check(PieceColor::Black));
        assert_eq!(b.evaluate(), i32::MAX);
    }

    #[test]
    fn mate_in_1_depth_3() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let m = b.minimax(3);
        assert!(m.1 == i32::MAX);
        b.make(m.0.unwrap());
        assert!(b.check(PieceColor::Black));
        assert_eq!(b.evaluate(), i32::MAX);
    }

    #[test]
    fn mate_in_3_endgame() {
        let mut b = BoardState::from_fen("7R/2N1P3/8/8/8/8/k6K/8 w").unwrap();
        let m = b.minimax(5);
        assert!(m.1 == i32::MAX);
    }

    //This takes a long time, disable if not needed
    #[test]
    /*fn mate_in_3_depth_5() {
        let mut b = BoardState::from_fen("r5k1/1bp3pp/rp6/3N4/4P3/P4R2/6PP/5RK1 w").unwrap();
        let m = b.minimax(5);
        assert!(m.1 == i32::MAX);
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
            b.board.pseudo_legal_moves(Field::new(4, 5)).unwrap(),
            vec![Field::new(5, 4), Field::new(5, 5)],
        );
    }

    #[test]
    fn white_pawn_not_take_center() {
        let b = BoardState::from_fen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(4, 5)).unwrap(),
            vec![],
        );
    }

    #[test]
    fn white_pawn_take_right() {
        let b = BoardState::from_fen("rnbqkbnr/pppp1ppp/8/5p2/4P3/8/PPPP1PPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(4, 5)).unwrap(),
            vec![Field::new(5, 6), Field::new(5, 5)],
        );
    }

    #[test]
    fn black_pawn_take_left() {
        let b = BoardState::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR b").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(5, 5)).unwrap(),
            vec![Field::new(4, 4), Field::new(4, 5)],
        );
    }

    #[test]
    fn black_pawn_not_take_center() {
        let b = BoardState::from_fen("rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR b").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(5, 4)).unwrap(),
            vec![],
        );
    }

    #[test]
    fn black_pawn_take_right() {
        let b = BoardState::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR b").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(5, 4)).unwrap(),
            vec![Field::new(4, 4), Field::new(4, 5)],
        );
    }

    #[test]
    fn white_pawn_double_push() {
        let b = BoardState::new();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(2, 1)).unwrap(),
            vec![Field::new(3, 1), Field::new(4, 1)],
        );
    }

    #[test]
    fn black_pawn_double_push() {
        let b = BoardState::new();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(7, 1)).unwrap(),
            vec![Field::new(6, 1), Field::new(5, 1)],
        );
    }

    #[test]
    fn white_pawn_double_push_blocked() {
        let b =
            BoardState::from_fen("rnbqkbnr/1ppppppp/8/8/p7/8/PPPPPPPP/RNBQKBNR w KQkq").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(2, 1)).unwrap(),
            vec![Field::new(3, 1)],
        );
    }

    #[test]
    fn black_pawn_double_push_blocked() {
        let b = BoardState::from_fen("rnbqkbnr/pppppppp/8/P7/8/8/1PPPPPPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(7, 1)).unwrap(),
            vec![Field::new(6, 1)],
        );
    }

    #[test]
    fn white_pawn_not_take_white_pawn() {
        let b =
            BoardState::from_fen("rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(2, 4)).unwrap(),
            vec![Field::new(3, 4), Field::new(4, 4)],
        );
    }

    #[test]
    fn white_knight_takes() {
        let b = BoardState::from_fen("rnbqkbnr/pp1ppppp/8/8/8/2p5/PPPPPPPP/RNBQKBNR w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(1, 2)).unwrap(),
            vec![Field::new(3, 1), Field::new(3, 3)],
        );
    }

    #[test]
    fn black_knight_corner_blocked() {
        let b = BoardState::from_fen("n7/2p3k1/1p6/8/8/4K3/8/8 w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(8, 1)).unwrap(),
            vec![],
        );
    }

    #[test]
    fn black_knight_corner_takes() {
        let b = BoardState::from_fen("n7/2p3k1/1P6/8/8/4K3/8/8 w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(8, 1)).unwrap(),
            vec![Field::new(6, 2)],
        );
    }

    #[test]
    fn black_knight_edge_takes() {
        let b = BoardState::from_fen("8/1k3p2/7n/8/6P1/8/2K5/8 w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(6, 8)).unwrap(),
            vec![Field::new(8, 7), Field::new(5, 6), Field::new(4, 7)],
        );
    }

    #[test]
    fn black_knight_edge_blocked() {
        let b = BoardState::from_fen("6p1/1k3p2/7n/5p2/6p1/8/1K6/8 w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(6, 8)).unwrap(),
            vec![],
        );
    }

    #[test]
    fn white_bishop_attack_partially_blocked() {
        let b = BoardState::from_fen("8/8/8/4R3/1p6/2B4k/8/p6K w").unwrap();
        eq_fields(
            b.board.pseudo_legal_moves(Field::new(3, 3)).unwrap(),
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
    fn eq_fields(v1: Vec<Move>, mut v2: Vec<Field>) {
        let mut fields: Vec<Field> = v1.into_iter().map(|m| m.to).collect();
        fields.sort_unstable();
        v2.sort_unstable();
        assert_eq!(fields, v2);
    }
}
