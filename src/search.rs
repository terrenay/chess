use std::{
    collections::HashMap,
    sync::{
        mpsc::{self, Receiver},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant},
};

use crate::{
    evaluation::Evaluation, zobrist::ZobristState, BoardState, Move, PieceColor,
    TranspositionEntry, TRANSPOSITION_TABLE_SIZE,
};

impl BoardState {
    pub fn minimax_standalone(&mut self, depth: u32) -> (Option<Move>, Evaluation) {
        #[allow(unused_variables)]
        let (sender, receiver) = mpsc::channel();
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
                    // self.draw(true);
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
                        eprintln!("CHECKMATE! BLACK WINS!");
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
                    //self.draw(true);
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
                        eprintln!("CHECKMATE! WHITE WINS!");
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
                    for m in self.generate_moves(true) {
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
                        //self.draw(true);
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
                    for m in self.generate_moves(true) {
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
                        //self.draw(true);
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

    ///Only call this function at horizon nodes.
    ///
    ///Avoid the horizon effect. If there is an ongoing trade at a horizon node (eg: queen captures a pawn at horizon, but
    /// the queen could be recaptured one move beyond the horizon), search deeper until the position becomes quiet.
    ///
    /// This is implemented by generating all captures in the current position. If the capture could significantly change the
    /// evaluation, we recursively generate all possible captures in the following position.
    ///
    /// Captures may be ignored if it is highly unlikely that they would be beneficial (eg: queen captures pawn).
    #[allow(unused_variables)]
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
}
