//#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
mod evaluation;
mod zobrist;

use std::{
    collections::HashMap,
    fmt::{self, Display},
    sync::{
        mpsc::{self, Receiver},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant},
};

use colored::Colorize;
use evaluation::*;
use ndarray::prelude::*;
use zobrist::*;

const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq";
//const STARTING_POSITION: &str = "rnbqkbnr/1p2pppp/p2p4/2p5/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq"; //Sicilian after opening
//const STARTING_POSITION: &str = "rn1qk2r/p1ppbppp/bp2pn2/8/2PP4/1P3NP1/P2BPP1P/RN1QKB1R w KQkq"; //Queens indian

const TRANSPOSITION_TABLE_SIZE: usize = 1_048_583;

#[derive(Clone)]
pub struct BoardState {
    pub board: Board,
    pub turn: PieceColor,
    ply: u16,
    pub castling_rights: CastlingRights,
    zobrist: ZobristState,
    moves: Vec<Move>,
    pub taken: Vec<Piece>,
    pub hash_history: Vec<u64>,
}

impl BoardState {
    pub fn new() -> Self {
        Self::from_fen(STARTING_POSITION).unwrap()
    }

    ///Get index to transposition table, depending on TRANSPOSITION_TABLE_SIZE
    fn transposition_table_index(&self) -> u32 {
        (self.zobrist.hash % TRANSPOSITION_TABLE_SIZE as u64) as u32
    }

    ///Static evaluation
    ///
    /// Positive: White's advantage
    pub fn evaluate(&mut self) -> Evaluation {
        evaluation::evaluate(self)
    }

    pub fn minimax_standalone(&mut self, depth: u32) -> (Option<Move>, Evaluation) {
        let (_, receiver) = mpsc::channel();
        self.minimax(
            depth,
            &mut HashMap::<u32, TranspositionEntry>::with_capacity(TRANSPOSITION_TABLE_SIZE),
            &receiver,
        )
    }

    ///Assumes root has color state.turn
    pub fn minimax(
        &mut self,
        depth: u32,
        transposition_table: &mut HashMap<u32, TranspositionEntry>,
        stop_receiver: &Receiver<bool>,
    ) -> (Option<Move>, Evaluation) {
        let mut best_move = None;
        let mut alpha = Evaluation::Mate(PieceColor::Black, 0);
        let mut beta = Evaluation::Mate(PieceColor::White, 0);
        let mut moves = 0;
        match self.turn {
            PieceColor::White => {
                let mut best_eval = Evaluation::Mate(PieceColor::Black, 0);
                for m in self.generate_moves(true) {
                    //todo: borrow this generated move as much as possible, don't copy it around! It's now on the heap (in a vector), so we can refer to it.
                    match stop_receiver.try_recv() {
                        Err(mpsc::TryRecvError::Disconnected) | Ok(_) => {
                            return (best_move, best_eval);
                        }
                        _ => (),
                    }
                    self.make(&m);
                    debug_assert_eq!(self.zobrist.hash, ZobristState::from_board_state(self).hash);
                    if !self.check(self.turn.opposite()) {
                        best_move.get_or_insert(m.clone());
                        let score = if self.threefold_repetition() {
                            //println!("3f rep in root");
                            //Evaluation::Draw
                            panic!()
                        } else {
                            self.minimax_helper(
                                depth - 1,
                                alpha,
                                beta,
                                transposition_table,
                                stop_receiver,
                            )
                        };
                        moves += 1;
                        if score > best_eval {
                            best_move = Some(m);
                            best_eval = score;
                        }
                        if score > alpha {
                            alpha = score;
                        }
                    }
                    self.unmake();
                }
                if moves == 0 {
                    if self.checkmate_given_zero_moves(PieceColor::Black) {
                        println!("CHECKMATE! BLACK WINS!");
                        return (None, Evaluation::Mate(PieceColor::Black, 0));
                    } else {
                        println!("STALEMATE! THE GAME IS A DRAW!");
                        return (None, Evaluation::Draw);
                    }
                }
                if self.taken.len() == 30 {
                    println!("STALEMATE! THE GAME IS A DRAW!");
                    return (None, Evaluation::Draw);
                }
                (best_move, best_eval)
            }

            //Black is the minimizing player
            PieceColor::Black => {
                let mut best_eval = Evaluation::Mate(PieceColor::White, 0);
                for m in self.generate_moves(true) {
                    match stop_receiver.try_recv() {
                        Err(mpsc::TryRecvError::Disconnected) | Ok(_) => {
                            return (best_move, best_eval);
                        }
                        _ => (),
                    }
                    self.make(&m);
                    debug_assert_eq!(self.zobrist.hash, ZobristState::from_board_state(self).hash);
                    if !self.check(self.turn.opposite()) {
                        best_move.get_or_insert(m.clone());
                        let score = if self.threefold_repetition() {
                            // println!("3f rep in root");
                            //Evaluation::Draw
                            panic!()
                        } else {
                            self.minimax_helper(
                                depth - 1,
                                alpha,
                                beta,
                                transposition_table,
                                stop_receiver,
                            )
                        };
                        moves += 1;
                        if score < best_eval {
                            best_move = Some(m.clone());
                            best_eval = score;
                        }
                        if score < beta {
                            beta = score;
                        }
                    }
                    self.unmake();
                }
                if moves == 0 {
                    if self.checkmate_given_zero_moves(PieceColor::White) {
                        println!("CHECKMATE! WHITE WINS!");
                        return (None, Evaluation::Mate(PieceColor::White, 0));
                    } else {
                        println!("STALEMATE! THE GAME IS A DRAW!");
                        return (None, Evaluation::Draw);
                    }
                }

                if self.taken.len() == 30 {
                    println!("STALEMATE! THE GAME IS A DRAW!");
                    return (None, Evaluation::Draw);
                }
                (best_move, best_eval)
            }
        }
    }

    ///If a checkmate is found, increases the distance to it by 1 (such that the number of remaining moves
    /// remains consistent).
    fn minimax_helper(
        &mut self,
        depth: u32,
        mut alpha: Evaluation,
        mut beta: Evaluation,
        transposition_table: &mut HashMap<u32, TranspositionEntry>,
        stop_receiver: &Receiver<bool>,
    ) -> Evaluation {
        if depth == 0 {
            self.quiescent_search(alpha, beta, transposition_table, stop_receiver)
        } else {
            let mut moves = 0;
            match self.turn {
                //White is the maximizing player
                PieceColor::White => {
                    let mut best_eval = Evaluation::Mate(PieceColor::Black, 0);
                    for m in self.generate_moves(depth > 3) {
                        match stop_receiver.try_recv() {
                            Err(mpsc::TryRecvError::Disconnected) | Ok(_) => {
                                return best_eval;
                            }
                            _ => (),
                        }
                        // println!("{}: white try move {}", depth, m);
                        //Branch cut: Don't explore this subtree further if it is already
                        //obvious that this variant will not be taken.
                        if beta <= alpha {
                            // println!("beta <= alpha: return");
                            return best_eval.increment_if_mate();
                        }
                        self.make(&m);
                        debug_assert_eq!(
                            self.zobrist.hash,
                            ZobristState::from_board_state(self).hash
                        );
                        if !self.check(self.turn.opposite()) {
                            let score = if self.threefold_repetition() {
                                //println!("3f rep in helper");

                                Evaluation::Draw
                            } else {
                                self.minimax_helper(
                                    depth - 1,
                                    alpha,
                                    beta,
                                    transposition_table,
                                    stop_receiver,
                                )
                            };
                            moves += 1;
                            if score > best_eval {
                                best_eval = score;
                            }
                            if score > alpha {
                                alpha = score;
                            }
                        }
                        self.unmake();
                    }
                    if (moves == 0 && !self.checkmate_given_zero_moves(PieceColor::Black))
                        || self.taken.len() == 30
                    {
                        best_eval = Evaluation::Draw;
                    }
                    best_eval.increment_if_mate()
                }
                //Black is the minimizing player
                PieceColor::Black => {
                    let mut best_eval = Evaluation::Mate(PieceColor::White, 0);
                    for m in self.generate_moves(depth > 3) {
                        match stop_receiver.try_recv() {
                            Err(mpsc::TryRecvError::Disconnected) | Ok(_) => {
                                return best_eval;
                            }
                            _ => (),
                        }
                        if beta <= alpha {
                            // println!("beta <= alpha: return");
                            return best_eval.increment_if_mate();
                        }
                        self.make(&m);
                        debug_assert_eq!(
                            self.zobrist.hash,
                            ZobristState::from_board_state(self).hash
                        );
                        if !self.check(self.turn.opposite()) {
                            let score = if self.threefold_repetition() {
                                // println!("3f rep in helper");

                                Evaluation::Draw
                            } else {
                                self.minimax_helper(
                                    depth - 1,
                                    alpha,
                                    beta,
                                    transposition_table,
                                    stop_receiver,
                                )
                            };
                            moves += 1;
                            if score < best_eval {
                                best_eval = score;
                            }
                            if score < beta {
                                beta = score;
                            }
                        }
                        self.unmake();
                    }
                    if (moves == 0 && !self.checkmate_given_zero_moves(PieceColor::White))
                        || self.taken.len() == 30
                    {
                        best_eval = Evaluation::Draw;
                    }
                    best_eval.increment_if_mate()
                }
            }
        }
    }

    ///Ensures minimax is only called with even depths. Never takes longer than time_limit.
    /// This function keeps an internal transposition table that is preserved throughout all iterative calls
    /// from this root position.
    ///
    /// Deeper searches order their moves based on results of shallower searches that are stored in the
    /// transposition table, which enables alpha-beta-pruning to remove a lot more subtrees, thus
    /// increasing the search speed.
    pub fn iterative_deepening(&self, time_limit_millis: u64) -> (Option<Move>, Evaluation) {
        let mut res = (None, Evaluation::Value(42)); //default not used because the loop always runs at least once
        let mut depth = 1; //Root is frontier node. All children (after all of root's possible moves) are evaluated with eval()
        let max_duration = Duration::from_millis(time_limit_millis);
        let start = Instant::now();

        /*Transposition Table: Zobrist keys are 64 bits. A table with 2^64 entries is way too big to fit into memory, so
        we modulo the key with the table size 2^20+7 to get the index. This leads to two types of collisions: Zobrist collisions
        and index collisions. We avoid index collisions by storing the full 64 bit Zobrist key inside a TranspositionEntry object.
        We don't have a way of avoiding Zobrist collisions and just hope they happen rarely. */

        /*We want to keep the transposition table between loop iterations, since that is the whole point of using it.
        However, since the minimax algorithm is run by a different thread, Rust doesn't allow us to simply use the table as if it
        were running in a single-threaded environment. We don't want to copy the whole table between each iteration, so we use
        a mutex. */

        /*Problem: One table needs about 60MB. Once the master threads times out, the worker thread is NOT killed but
        continues running in the background (but its result is finally discarded) - which wastes CPU and RAM.
        I need to somehow signal to the minimax thread when it is allowed to stop.*/

        let table_lock = Arc::new(Mutex::new(
            HashMap::<u32, TranspositionEntry>::with_capacity(TRANSPOSITION_TABLE_SIZE),
        ));

        'outer: loop {
            let (sender, receiver) = mpsc::channel();
            let (stop_sender, stop_receiver) = mpsc::channel();
            let mut board_clone = self.clone();
            let table_lock_clone = Arc::clone(&table_lock);

            thread::spawn(move || {
                let mut table = table_lock_clone.lock().unwrap();
                let worker_res = board_clone.minimax(depth, &mut table, &stop_receiver);
                sender.send(worker_res);
            });

            loop {
                //|| matches!(res.1, Evaluation::Mate(_, _))
                if start.elapsed() >= max_duration && depth > 1 {
                    stop_sender.send(true).unwrap();
                    break 'outer;
                }

                if let Ok(worker_res) = receiver.try_recv() {
                    res = worker_res;
                    //let table = Arc::clone(&table_lock);
                    //let table = table.lock().unwrap();
                    println!(
                        "Depth {} gives {} with an expected eval of {:#?}.",
                        depth,
                        res.0.clone().unwrap(),
                        res.1
                    );
                    //todo!("abbrechen wenn mate gefunden");
                    /*println!(
                        "After depth {}, transposition table contains {} entries.\n",
                        depth,
                        table.len()
                    );*/
                    break;
                }
            }

            //Only break if we have fully searched at least to a depth of 1.
            depth += 1;
        }

        println!(
            "Searched to a depth of {} plies in {} ms.",
            depth - 1,
            start.elapsed().as_millis()
        );

        res
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

    ///True if the last move leads to a threefold repetition. Might return true even though there is no threefold repetition.
    fn threefold_repetition(&self) -> bool {
        let last = self.hash_history.last().unwrap();
        self.hash_history.iter().filter(|&n| n == last).count() >= 3
    }

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
            self.make(&m);
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

    pub fn generate_moves(&self, sort: bool) -> Vec<Move> {
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
        v = self.avoid_touching_kings(v);
        /*Im early bis midgame pawn-bishop-knight-queen-rook-king.
        Nur bei root sortieren (in jedem child sortieren braucht zu viel zeit für wenig
        benefit. dort nur vorherige best moves aus transposition table berücksichtigen.) */
        if sort {
            v.sort();
        }
        v
    }

    ///Only called in make. To unmake the hash, we just pop the last value from the hash history.
    fn make_zobrist(
        &mut self,
        m: &Move,
        moving_piece: Piece,
        changed_castling_rights: &ChangedCastlingRights,
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
                    panic!("MoveType attack but empty target");
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

        //Change turn

        self.zobrist.change_black_to_move();
    }

    ///Assumes the move is fully legal!
    pub fn make(&mut self, m: &Move) {
        self.moves.push(m.clone());
        let old_castling_rights = self.castling_rights.clone();
        if let Square::Full(moving_piece) = self.board.at(m.from.rank, m.from.file) {
            match m.move_type {
                MoveType::Default => {}

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

            let changed_castling_rights =
                ChangedCastlingRights::new(&old_castling_rights, &self.castling_rights);

            //eprintln!("make calles make_zobrist");
            self.make_zobrist(m, moving_piece, &changed_castling_rights);

            self.hash_history.push(self.zobrist.hash);

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
    }

    ///Unmakes the last move (the one at the end of the moves vec)
    pub fn unmake(&mut self) {
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

                //To update the zobrist values for the unmake case, we first unmake the move. Then we pretend that we
                //MAKE the move and update the zobrist values accordingly! Since the xor operation is symmetric, this has to have
                //the same effect.
                self.hash_history.pop();
                self.zobrist.hash = *self.hash_history.last().unwrap();
            } else {
                panic!("No piece at destination in unmake");
            }
        } else {
            panic!("Trying to unmake but there is no previous move");
        }
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
        let piece = Piece::king(self.turn);
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
                    piece,
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
                    piece,
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
                    piece,
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
                    piece,
                    MoveType::CastleQueenside,
                ));
            }
        }
        v
    }

    ///Given pseudo-legal moves of all pices, remove those that would put the two kings too close to each other
    /// and remove those that would take a king.
    fn avoid_touching_kings(&self, v: Vec<Move>) -> Vec<Move> {
        let opponent_king = match self.turn {
            PieceColor::White => self.board.black_king,
            PieceColor::Black => self.board.white_king,
        };

        let my_king = match self.turn {
            PieceColor::White => self.board.white_king,
            PieceColor::Black => self.board.black_king,
        };

        v.into_iter()
            .filter(|m| {
                //We don't want the kings too close
                (if m.from == my_king {
                    (m.to.rank - opponent_king.rank).abs() > 1
                        || (m.to.file - opponent_king.file).abs() > 1
                } else {
                    true
                }) && m.to != my_king //and we don't want to take any kings
                    && m.to != opponent_king
            })
            .collect()
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
                    if let Some(last) = self.moves.last() {
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

    ///Standard: Assume no castling rights
    fn from_fen(fen: &str) -> Result<BoardState, Error> {
        let chars = &mut fen.chars();

        let board = Self::parse_board_fen(chars)?;

        let turn = Self::parse_turn_fen(chars)?;

        chars.by_ref().next(); //Consume the whitespace if it's there

        let castling_rights = parse_castling_rights_fen(chars, &board)?;

        let zobrist = ZobristState::from(&board, turn, &castling_rights);

        let hashes = vec![zobrist.hash];

        Ok(Self {
            board,
            turn,
            ply: 1,
            castling_rights,
            zobrist,
            moves: vec![],
            taken: vec![],
            hash_history: hashes,
        })
    }

    ///Only call this function at horizon nodes.
    ///
    ///Avoid the horizon effect. If there is an ongoing trade at a horizon node (eg: queen captures a pawn at horizon, but
    /// the queen could be recaptured one move beyond the horizon), search deeper until the position becomes quiet.
    ///
    /// This is implemented by generating all captures in the current position. If the capture could significantly change the
    /// evaluation, we recursively generate all possible captures in the following position.
    ///
    /// Captures may be ignored if it is highly unlikely that they would be beneficial (eg: queen captures pawn).
    fn quiescent_search(
        &mut self,
        alpha: Evaluation,
        beta: Evaluation,
        transposition_table: &mut HashMap<u32, TranspositionEntry>,
        stop_receiver: &Receiver<bool>,
    ) -> Evaluation {
        let eval = match transposition_table.get(&self.transposition_table_index()) {
            Some(entry) => {
                if entry.zobrist_key == self.zobrist.hash {
                    entry.eval
                } else {
                    let eval = self.evaluate();
                    //Replacement scheme: Always
                    let entry = TranspositionEntry::new(self.zobrist.hash, eval);
                    transposition_table.insert(self.transposition_table_index(), entry);
                    eval
                }
            }
            //todo: make it less ugly
            None => {
                let eval = self.evaluate();
                let entry = TranspositionEntry::new(self.zobrist.hash, eval);
                transposition_table.insert(self.transposition_table_index(), entry);
                eval
            }
        };
        //increment_if_mate nur vor returnen aufrufen. Im table ohne dem speichern.
        eval.increment_if_mate()
    }
}

///Consumes trailing whitespace, if any.
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
#[derive(Clone, PartialEq, Eq)]
pub struct Move {
    pub from: Field,
    pub to: Field,
    piece: Piece,
    move_type: MoveType,
    promotion: Option<Piece>,
}

impl Move {
    const fn new(from: Field, to: Field, piece: Piece, move_type: MoveType) -> Self {
        Self {
            from,
            to,
            piece,
            move_type,
            promotion: None,
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
        write!(f, "{}{}", self.from, self.to)
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

    ///May return NoPieceOnField Error
    fn pseudo_legal_moves(&self, from: Field) -> Result<Vec<Move>, Error> {
        let Field { rank, file } = from;
        match self.at(rank, file) {
            Square::Full(p) => match p.kind {
                PieceKind::Pawn => Ok(self.pseudo_legal_pawn_moves(p.color, from)),
                PieceKind::Knight => Ok(self.pseudo_legal_knight_moves(p.color, from)),
                PieceKind::Bishop => Ok(self.pseudo_legal_bishop_moves(p.color, from)),
                PieceKind::King => Ok(self.pseudo_legal_king_moves(p.color, from)),
                PieceKind::Rook => Ok(self.pseudo_legal_rook_moves(p.color, from)),
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
        let pawn = Piece::pawn(color);

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
                    pawn,
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
                            pawn,
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
                    pawn,
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
                    pawn,
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
        self.filter_free_or_opponent(from, v, Piece::knight(color))
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
        self.filter_free_or_opponent(from, v, Piece::king(color))
    }

    fn pseudo_legal_rook_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let Field { rank, file } = from;
        let piece = Piece::rook(color);
        //Upwards
        let mut v = vec![];
        for i in rank + 1..=8 {
            let to = Field::new(i, file);
            if self.insert_or_break_loop(from, to, piece, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Downwards
        for i in (1..rank).rev() {
            let to = Field::new(i, file);
            if self.insert_or_break_loop(from, to, piece, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Right
        for j in file + 1..=8 {
            let to = Field::new(rank, j);
            if self.insert_or_break_loop(from, to, piece, &mut v) == BreakLoop::True {
                break;
            }
        }
        //Left
        for j in (1..file).rev() {
            let to = Field::new(rank, j);
            if self.insert_or_break_loop(from, to, piece, &mut v) == BreakLoop::True {
                break;
            }
        }
        v
    }

    fn pseudo_legal_bishop_moves(&self, color: PieceColor, from: Field) -> Vec<Move> {
        let mut v = vec![];
        let mut diff = 1;
        let Field { rank, file } = from;
        let piece = Piece::bishop(color);

        //Up right
        while rank + diff <= 8 && file + diff <= 8 {
            let to = Field::new(rank + diff, file + diff);
            if self.insert_or_break_loop(from, to, piece, &mut v) == BreakLoop::True {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Up left
        while rank + diff <= 8 && file - diff >= 1 {
            let to = Field::new(rank + diff, file - diff);
            if self.insert_or_break_loop(from, to, piece, &mut v) == BreakLoop::True {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Down right
        while rank - diff >= 1 && file + diff <= 8 {
            let to = Field::new(rank - diff, file + diff);
            if self.insert_or_break_loop(from, to, piece, &mut v) == BreakLoop::True {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Down left
        while rank - diff >= 1 && file - diff >= 1 {
            let to = Field::new(rank - diff, file - diff);
            if self.insert_or_break_loop(from, to, piece, &mut v) == BreakLoop::True {
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
        from: Field,
        to: Field,
        piece: Piece,
        v: &mut Vec<Move>,
    ) -> BreakLoop {
        let Field { rank, file } = to;
        match self.at(rank, file) {
            Square::Empty => {
                v.push(Move::new(from, to, piece, MoveType::Default));
            }
            Square::Full(p) => {
                if p.color == piece.color.opposite() {
                    v.push(Move::new(from, to, piece, MoveType::Capture));
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
    fn filter_free_or_opponent(&self, from: Field, v: Vec<Field>, piece: Piece) -> Vec<Move> {
        let mut default_moves: Vec<Move> = v
            .iter()
            .filter(|to| matches!(self.at(to.rank, to.file), Square::Empty))
            .map(|&to| Move::new(from, to, piece, MoveType::Default))
            .collect();

        let captures: Vec<Move> = v
            .into_iter()
            .filter(|&to| {
                if let Square::Full(p) = self.at(to.rank, to.file) {
                    p.color == piece.color.opposite()
                } else {
                    false
                }
            })
            .map(|to| Move::new(from, to, piece, MoveType::Capture))
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

///None means no move has been stored, it does not imply checkmate!
pub struct TranspositionEntry {
    zobrist_key: u64,
    eval: Evaluation,
    //eval_type: EvaluationType,
    //best_move: Option<Move>,
}

impl TranspositionEntry {
    pub fn new(
        zobrist_key: u64,
        eval: Evaluation,
        //eval_type: EvaluationType,
        //best_move: Option<Move>,
    ) -> Self {
        Self {
            zobrist_key,
            eval,
            //eval_type,
            // best_move,
        }
    }
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Debug)]
///Used to store which castling rights have been lost by this move
struct ChangedCastlingRights {
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
        assert_eq!(b.evaluate(), Evaluation::Mate(PieceColor::White, 0));
    }

    #[test]
    fn mate_in_1_depth_1() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let m = b.minimax_standalone(1);
        assert!(m.1 == Evaluation::Mate(PieceColor::White, 1));
        b.make(&m.0.unwrap());
        assert!(b.check(PieceColor::Black));
        assert_eq!(b.evaluate(), Evaluation::Mate(PieceColor::White, 0));
    }

    #[test]
    fn mate_in_1_depth_2() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let (m, eval) = b.minimax_standalone(2);
        let m = m.unwrap();
        println!("got {}, {:#?}", m, eval);
        assert!(eval == Evaluation::Mate(PieceColor::White, 1));
        b.make(&m);
        assert!(b.check(PieceColor::Black));
        assert_eq!(b.evaluate(), Evaluation::Mate(PieceColor::White, 0));
    }

    //Achtung falls verhalten sich ändert mit hashtable, weil hier castling rights in der start-
    //position ignoriert werden
    #[test]
    fn mate_in_1_depth_3() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let (m, eval) = b.minimax_standalone(3);
        let m = m.unwrap();
        assert!(eval == Evaluation::Mate(PieceColor::White, 1));
        b.make(&m);
        assert!(b.check(PieceColor::Black));
        assert_eq!(b.evaluate(), Evaluation::Mate(PieceColor::White, 0));
        println!("{}", m);
    }

    #[test]
    fn mate_in_1_iterative() {
        let mut b = BoardState::from_fen("1k6/ppp3Q1/8/8/8/8/6K1/8 w").unwrap();
        let (m, eval) = b.iterative_deepening(500);
        let m = m.unwrap();
        assert!(eval == Evaluation::Mate(PieceColor::White, 1));
        b.make(&m);
        assert!(b.check(PieceColor::Black));
        assert_eq!(b.evaluate(), Evaluation::Mate(PieceColor::White, 0));
        println!("{}", m);
    }

    #[test]
    fn mate_in_3_endgame() {
        let mut b = BoardState::from_fen("7R/2N1P3/8/8/8/8/k6K/8 w").unwrap();
        let m = b.minimax_standalone(5);
        assert_eq!(m.0.unwrap().to, Field::new(8, 2));
    }

    #[test]
    fn mate_in_3_with_rem_count() {
        let mut b = BoardState::from_fen("8/p1p2pp1/4k3/8/P1n2PPp/3K3P/3p4/1r6 b").unwrap();
        let m = b.minimax_standalone(5);
        assert_eq!(m.1, Evaluation::Mate(PieceColor::Black, 5));
    }

    /*#[test]
    fn mate_in_3_iterative_long() {
        let b = BoardState::from_fen("7R/2N1P3/8/8/8/8/k6K/8 w").unwrap();
        let m = b.iterative_deepening(5000);
        assert_eq!(m.0.unwrap().to, Field::new(8, 2));
    }*/

    //This takes a long time, disable if not needed
    //#[test]
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
        //soll nur schauen, dass der hash mal deterministisch ist.

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
