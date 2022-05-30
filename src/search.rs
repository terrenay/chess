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
    evaluation::{end_of_game, evaluate_rel, EvaluationFlag},
    zobrist::ZobristState,
    BoardState, Move, MoveType, PieceColor, TranspositionEntry, TRANSPOSITION_TABLE_SIZE,
};

impl BoardState {
    /// best_eval entspricht einem neuen alpha, wenn man die aktuelle node als root nehmen würde
    /// Relativ zu self.turn
    pub fn negamax(
        &mut self,
        depth: u32,
        mut alpha: i32,
        beta: i32,
        transposition_table: &mut HashMap<u32, TranspositionEntry>,
        stop_receiver: &Receiver<bool>,
    ) -> (Vec<Move>, i32) {
        // eprintln!("--Start depth {}. a: {}. b: {}--", depth, alpha, beta);

        /*Accessing the transposition table */

        if let Some(entry) = self.get_transposition_entry(transposition_table) {
            if entry.depth >= depth {
                if entry.flag == EvaluationFlag::Exact {
                    let rel_eval = if self.turn == PieceColor::White {
                        entry.eval
                    } else {
                        -entry.eval
                    };
                    if entry.best_move.is_some() {
                        // eprintln!("From tt");
                        return (vec![entry.best_move.clone().unwrap()], rel_eval);
                    } else {
                        // eprintln!("in tt but no move");
                    }
                }
            }
        }

        let legal_moves = self.generate_legal_moves(true, false);

        let (end_of_game_eval, legal_moves) = end_of_game(self, Some(legal_moves));

        if let Some(v) = end_of_game_eval {
            return (vec![], v);
        } else if depth == 0 {
            let res = self.quiescence_search_rel(
                10,
                alpha,
                beta,
                legal_moves,
                transposition_table,
                stop_receiver,
            );

            let flag = if res.1 <= alpha {
                EvaluationFlag::LowerBound
            } else if res.1 >= beta {
                EvaluationFlag::UpperBound
            } else {
                EvaluationFlag::Exact
            };

            let best_move = if res.0.is_empty() {
                None
            } else {
                assert_eq!(flag, EvaluationFlag::Exact);
                Some(res.0.first().unwrap().clone())
            };

            let entry = TranspositionEntry::new(self.zobrist.hash, 0, res.1, flag, best_move);
            self.update_transposition_table(transposition_table, entry);
            return res;
        }

        let mut best_line = vec![];
        let mut best_eval = -i32::MAX;
        let legal_moves = legal_moves.unwrap();
        debug_assert!(!legal_moves.is_empty());

        for m in legal_moves {
            match stop_receiver.try_recv() {
                Err(mpsc::TryRecvError::Disconnected) | Ok(_) => {
                    return (vec![], best_eval);
                }
                _ => (),
            }

            self.make(&m);
            debug_assert_eq!(self.zobrist.hash, ZobristState::from_board_state(self).hash);

            if best_line.is_empty() {
                best_line.push(m.clone());
            }
            //self.draw_board(true);
            let (new_line, new_eval) =
                self.negamax(depth - 1, -beta, -alpha, transposition_table, stop_receiver);
            let new_eval = -new_eval; //negate opponent's evaluation

            if new_eval > best_eval {
                // eprintln!("Depth {}, best_eval now {}", depth, new_eval);
                best_eval = new_eval;
                best_line = new_line;
                best_line.push(m);
                /*eprintln!("Updated best_line to: ");
                for v in best_line.iter() {
                    eprint!("{} - ", v);
                }
                eprintln!();*/
            }

            /*If this move leads to a position with a better value than beta, we can stop.
            We know that the minimizing opponent has a forced way to reach a position with an evaluation of beta, so if he plays
            optimally, he will not play anything that would lead to the current position (since it would lead to a worse position
            for him than what he can already achieve by some different move) */

            /*If this move increases alpha, we store it as our new best move. */

            if new_eval >= beta {
                // eprintln!("Move too good, outside beta cutoff");
                self.unmake();
                break;
            } else if new_eval > alpha {
                // eprintln!("alpha updated");
                alpha = new_eval;
            }

            self.unmake();
        }
        /*  eprintln!("--End depth {}. Return line: --", depth);
        for v in best_line.iter() {
            eprint!("{} - ", v);
        }*/

        (best_line, best_eval)
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
    ///
    /// Returns at least alpha and at most beta (both inclusive)
    fn quiescence_search_rel(
        &mut self,
        depth: u32,
        mut alpha: i32,
        beta: i32,
        legal_moves_from_negamax: Option<Vec<Move>>,
        transposition_table: &mut HashMap<u32, TranspositionEntry>,
        stop_receiver: &Receiver<bool>,
    ) -> (Vec<Move>, i32) {
        //todo: Problem: für checkmate / draw muss ich alle moves generieren, aber dann verliere ich den ganzen vorteil der
        //quiescence search (die ja nur captures generieren müsste)
        /*lösung: könnte noch ein zweites argument mit legal_captures machen. problem ist aktuell nur noch, dass ich für stalemate
        trotzdem alle moves generieren muss. auch ein legaler capture reicht als gegenbeispiel, dann muss ich nicht extra alle
        pseudo-legal moves generieren und dann einen davon maken. */

        /*The current evaluation is a lower bound on the final score. This assumes the null move hypothesis, which states
        that there is always a playable move that increases one's score. Thus, if we don't find a capture that raises alpha,
        we assume there is some non-capture move that would raise it anyway and return the standing_pat score.*/

        /*Small optimization: When quiescence is first called from negamax, it has already determined that the current position
        is not end of game (otherwise, it would terminate instead of calling quiescence). In addition, it has already
        generated all legal moves for the current position. This saves us from generating them again.
        Therefore, if and only if this function is called from negamax, legal_moves is Some.*/

        let legal_moves;

        match legal_moves_from_negamax {
            Some(m) => legal_moves = Some(m),
            None => {
                let (end_of_game_eval, m) = end_of_game(self, None);
                legal_moves = m;
                if let Some(v) = end_of_game_eval {
                    return (vec![], v);
                }
            }
        }

        /*Since we know the position is not checkmate or draw, we don't compute that again in the static evaluation. */

        let standing_pat = evaluate_rel(self, true);

        /*If this position is so good it can't be reached, we don't consider it further. */

        if standing_pat >= beta {
            return (vec![], beta);
        }

        /*Still remember the beta cutoff. */

        if depth == 0 {
            return (vec![], Ord::min(standing_pat, beta));
        }

        /*If this position improves alpha. */

        if standing_pat > alpha {
            alpha = standing_pat;
        }

        let mut best_line = vec![];

        /*Either the checkmate verification already computed all legal moves, or we compute only captures. */

        let captures = match legal_moves {
            Some(legal_moves) => legal_moves
                .into_iter()
                .filter(|m| m.move_type == MoveType::Capture)
                .collect(),
            None => self.generate_legal_moves(true, true),
        };

        for m in captures {
            match stop_receiver.try_recv() {
                Err(mpsc::TryRecvError::Disconnected) | Ok(_) => {
                    return (best_line, alpha);
                }
                _ => (),
            }

            self.make(&m);
            debug_assert_eq!(self.zobrist.hash, ZobristState::from_board_state(self).hash);

            let (new_line, score) = self.quiescence_search_rel(
                depth - 1,
                -beta,
                -alpha,
                None,
                transposition_table,
                stop_receiver,
            );
            let score = -score;

            //If this line quiets down in a position with a score greater than beta, the opponent will not
            //play a move that would lead to the current position, so we are done.

            if score >= beta {
                self.unmake();
                return (new_line, beta);
            }

            //If this line is an improvement over the previous best line:

            if score > alpha {
                alpha = score;
                best_line = new_line;
                best_line.push(m);
            }

            self.unmake();
        }

        (best_line, alpha)
    }

    /// Never takes longer than time_limit.
    /// This function keeps an internal transposition table that is preserved throughout all iterative calls
    /// from this root position.
    ///
    /// Deeper searches order their moves based on results of shallower searches that are stored in the
    /// transposition table, which enables alpha-beta-pruning to remove a lot more subtrees, thus
    /// increasing the search speed.
    pub fn iterative_deepening_nega(
        &self,
        time_limit_millis: u64,
        min_depth: Option<u32>,
    ) -> (Vec<Move>, i32) {
        //todo!("mit option eine minimum depth angeben und als standalone benutzen");
        let mut res = (vec![], 42); //default not used because the loop always runs at least once
        let mut depth = 1; //Root is frontier node. All children (after all of root's possible moves) are evaluated
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
                let mut worker_res =
                    board_clone.negamax(depth, -i32::MAX, i32::MAX, &mut table, &stop_receiver);
                worker_res.0.reverse();
                sender.send(worker_res);
            });

            loop {
                if start.elapsed() >= max_duration && depth > min_depth.or(Some(1)).unwrap() {
                    stop_sender.send(true).unwrap();
                    break 'outer;
                }

                if let Ok(worker_res) = receiver.try_recv() {
                    res = worker_res;
                    //let table = Arc::clone(&table_lock);
                    //let table = table.lock().unwrap();
                    println!("Depth {}: {:#?}.", depth, res.1);
                    for v in res.0.iter() {
                        print!("{} ", v);
                    }
                    println!();
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

            if res.1 == i32::MAX || res.1 == -i32::MAX {
                eprintln!("Stop deepening because forced mate has been found");
                break;
            }
        }

        println!(
            "Searched to a depth of {} plies in {} ms.",
            depth - 1,
            start.elapsed().as_millis()
        );

        res
    }
}
