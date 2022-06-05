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
    ) -> i32 {
        // eprintln!("--Start depth {}. a: {}. b: {}--", depth, alpha, beta);

        /*Accessing the transposition table. best_move is None only in horizon nodes and end_of_game nodes.
        Therefore */

        let legal_moves = self.generate_legal_moves(true, false, Some(transposition_table));

        let (end_of_game_eval, legal_moves) =
            end_of_game(self, Some(legal_moves), Some(transposition_table));

        if let Some(v) = end_of_game_eval {
            let entry = TranspositionEntry::new(
                self.zobrist.hash,
                depth,
                self.absolute_eval(v),
                EvaluationFlag::Exact,
                None,
            );
            self.update_transposition_table(transposition_table, entry);
            return v;
        }

        /*Unfortunately we have to check for end of game before getting values from the transposition table.
        That is because a position which has a threefold repetition draw incoming in the next move has the same
        zobrist hash as one without such a threat. Does getting the tt value from a position without the threat would
        lead to incorrect play and allow the opponent to force a draw. */
        if let Some(entry) =
            self.get_matching_transposition_entry(depth, EvaluationFlag::Exact, transposition_table)
        {
            /*eprintln!(
                "Get eval from TT (exact). Contains best_move: {:?}",
                entry.best_move
            );*/
            return self.relative_eval(entry.eval);
        }

        //todo schöner kombinieren
        if let Some(entry) = self.get_matching_transposition_entry(
            depth,
            EvaluationFlag::LowerBound,
            transposition_table,
        ) {
            /*eprintln!(
                "Get eval from TT (lower bound). Contains best_move: {:?}",
                entry.best_move
            );*/
            let rel_eval = self.relative_eval(entry.eval);
            if rel_eval >= beta {
                // eprintln!("tt better than beta->return");
                return beta;
            }
            // eprintln!("tt worse than beta.");
        }

        //todo schöner kombinieren
        if let Some(entry) = self.get_matching_transposition_entry(
            depth,
            EvaluationFlag::UpperBound,
            transposition_table,
        ) {
            /*eprintln!(
                "Get eval from TT (upper bound). Contains best_move: {:?}",
                entry.best_move
            );*/
            let rel_eval = self.relative_eval(entry.eval);
            if rel_eval <= alpha {
                // eprintln!("tt worse than alpha->return");
                /*bug fixed: Hier muss ich jetzt alpha returnen, sonst verbessert sich die aussage nicht. */
                return alpha;
            }
            // eprintln!("tt better than alpha");
        }

        if depth == 0 {
            debug_assert!(legal_moves.is_some());
            return self.quiescence_search_rel(
                10,
                alpha,
                beta,
                legal_moves,
                transposition_table,
                stop_receiver,
            );
        }

        let mut best_move = None;
        let mut best_eval = -i32::MAX;
        let legal_moves = legal_moves.unwrap();
        debug_assert!(!legal_moves.is_empty());

        //If no move improves alpha, we don't really know the evaluation. We only know it's worse than alpha, so we set the
        //upper bound flag, which means the evaluation stored is an upper bound on the real evaluation of this position.
        let mut flag = EvaluationFlag::UpperBound;

        for m in legal_moves {
            match stop_receiver.try_recv() {
                Err(mpsc::TryRecvError::Disconnected) | Ok(_) => {
                    return best_eval;
                }
                _ => (),
            }

            self.make(&m);
            debug_assert_eq!(self.zobrist.hash, ZobristState::from_board_state(self).hash);

            if best_move.is_none() {
                best_move = Some(m.clone());
            }

            // self.draw_board(true);
            let new_eval =
                -self.negamax(depth - 1, -beta, -alpha, transposition_table, stop_receiver);

            if new_eval > best_eval {
                // eprintln!("Depth {}, best_eval now {}", depth, new_eval);
                best_eval = new_eval;
                best_move = Some(m);
                // eprintln!();
            }

            /*If this move leads to a position with a better value than beta, we can stop.
            We know that the minimizing opponent has a forced way to reach a position with an evaluation of beta, so if he plays
            optimally, he will not play anything that would lead to the current position (since it would lead to a worse position
            for him than what he can already achieve by some different move) */

            /*If this move increases alpha, we store it as our new best move. */

            /*Possible error: shouldn't we return beta if evaluation is greater than beta? Like in quiescence? */

            if new_eval >= beta {
                // eprintln!("Move too good, outside beta cutoff");
                self.unmake();
                //If this move has an evaluation greater than beta, we don't know the real score. We only know it's at least beta.
                flag = EvaluationFlag::LowerBound;
                break;
            } else if new_eval > alpha {
                // eprintln!("alpha updated");
                alpha = new_eval;
                flag = EvaluationFlag::Exact;
            }

            self.unmake();
        }
        // eprintln!("--End depth {}. Return move: {:?}", depth, best_move);

        /*Update transposition table after all moves have been searched. */

        let entry = TranspositionEntry::new(
            self.zobrist.hash,
            depth,
            self.absolute_eval(best_eval),
            flag,
            best_move,
        );
        self.update_transposition_table(transposition_table, entry);

        best_eval
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
    ) -> i32 {
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

        /*eprintln!(
            "start quiescence depth {}, a: {}, b: {}",
            depth, alpha, beta
        );*/
        let legal_moves;
        let horizon_node = legal_moves_from_negamax.is_some();

        match legal_moves_from_negamax {
            Some(m) => legal_moves = Some(m),
            None => {
                let (end_of_game_eval, m) = end_of_game(self, None, Some(transposition_table));
                legal_moves = m;
                if let Some(v) = end_of_game_eval {
                    // eprintln!("quiescence says EOG");
                    if v > beta {
                        // eprintln!("return outside alpha beta");
                    }
                    self.quiescence_update_table(horizon_node, v, None, transposition_table);
                    return v;
                }
            }
        }

        /*Since we know the position is not checkmate or draw, we don't compute that again in the static evaluation. */

        let standing_pat = evaluate_rel(self, true);
        // eprintln!("standing pat: {}", standing_pat);

        /*If this position is so good it can't be reached, we don't consider it further. */

        if standing_pat >= beta {
            self.quiescence_update_table(horizon_node, beta, None, transposition_table);
            return beta;
        }

        /*Still remember the beta cutoff. */

        if depth == 0 {
            //hier kann tt nicht geupdatet werden, weil von nega nicht mit depth=0 gecalled wird. und wir wollen nur in quiescence
            //root den tt updaten
            return Ord::min(standing_pat, beta);
        }

        /*If this position improves alpha. */

        if standing_pat > alpha {
            // eprintln!("NMH: set alpha to {}", standing_pat);
            alpha = standing_pat;
        }

        /*Either the checkmate verification already computed all legal moves, or we compute only captures. */

        let captures = match legal_moves {
            Some(legal_moves) => legal_moves
                .into_iter()
                .filter(|m| m.move_type == MoveType::Capture)
                .collect(),
            None => self.generate_legal_moves(true, true, Some(transposition_table)),
        };

        //set to something if there is a capture
        let mut best_move = captures.first().cloned();

        for m in captures {
            match stop_receiver.try_recv() {
                Err(mpsc::TryRecvError::Disconnected) | Ok(_) => {
                    return alpha;
                }
                _ => (),
            }

            self.make(&m);
            debug_assert_eq!(self.zobrist.hash, ZobristState::from_board_state(self).hash);

            let score = -self.quiescence_search_rel(
                depth - 1,
                -beta,
                -alpha,
                None,
                transposition_table,
                stop_receiver,
            );

            //If this line quiets down in a position with a score greater than beta, the opponent will not
            //play a move that would lead to the current position, so we are done.

            if score >= beta {
                // eprintln!("{} better than beta ({})", score, beta);
                best_move = Some(m);
                self.unmake();
                self.quiescence_update_table(horizon_node, beta, best_move, transposition_table);
                return beta;
            }

            //If this line is an improvement over the previous best line (and over the horizon position):

            if score > alpha {
                // eprintln!("raise alpha");
                alpha = score;
                best_move = Some(m);
            } else {
                // eprintln!("worse than beta");
            }

            self.unmake();
        }

        /*Store horizon node's best move in transposition table. This is guaranteed to be a horizon node,
        because legal_moves is only Some if this function was called from negamax (which only calls quiescence at depth 0). */

        /*Correctness: I'm not sure whether flag should always be exact. I think we assume that this is like a static
        evaluation which is always correct, even though internally quiescence uses beta cutoffs as well. */

        self.quiescence_update_table(horizon_node, alpha, best_move, transposition_table);
        // eprintln!("end quiescence");
        alpha
    }

    fn quiescence_update_table(
        &self,
        horizon_node: bool,
        rel_eval: i32,
        best_move: Option<Move>,
        transposition_table: &mut HashMap<u32, TranspositionEntry>,
    ) {
        if horizon_node {
            let flag = EvaluationFlag::Exact;
            let eval = self.absolute_eval(rel_eval);
            let entry = TranspositionEntry::new(self.zobrist.hash, 0, eval, flag, best_move);
            // eprintln!("update table from horizon node:");
            self.update_transposition_table(transposition_table, entry);
        }
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
        time_limit_millis: Option<u64>,
        min_depth: Option<u32>,
    ) -> (i32, Option<Move>) {
        //todo!("mit option eine minimum depth angeben und als standalone benutzen");
        let mut eval = 42; //default not used because the loop always runs at least once
        let mut best_move = None;
        let mut depth = 1; //Root is frontier node. All children (after all of root's possible moves) are evaluated
        let max_duration = time_limit_millis.map(Duration::from_millis);
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

                let worker_eval =
                    board_clone.negamax(depth, -i32::MAX, i32::MAX, &mut table, &stop_receiver);

                /*let exact_set: Vec<&TranspositionEntry> = table
                    .values()
                    .filter(|&e| e.flag == EvaluationFlag::Exact)
                    .collect();
                let no_moves_exact = exact_set.iter().filter(|e| e.best_move.is_none()).count();
                let upper_set: Vec<&TranspositionEntry> = table
                    .values()
                    .filter(|&e| e.flag == EvaluationFlag::UpperBound)
                    .collect();
                let no_moves_upper = upper_set.iter().filter(|e| e.best_move.is_none()).count();
                let lower_set: Vec<&TranspositionEntry> = table
                    .values()
                    .filter(|&e| e.flag == EvaluationFlag::LowerBound)
                    .collect();
                let no_moves_lower = lower_set.iter().filter(|e| e.best_move.is_none()).count();

                println!("The table contains {} entries.", table.len());
                println!("The table contains {} exact entries.", exact_set.len());
                println!("Of which {} contain no move...", no_moves_exact);
                println!("The table contains {} upper bounds.", upper_set.len());
                println!("Of which {} contain no move...", no_moves_upper);
                println!("The table contains {} lower bounds.", lower_set.len());
                println!("Of which {} contain no move...", no_moves_lower);*/

                let worker_best_move = match board_clone.get_transposition_entry(&table) {
                    Some(entry) => entry.best_move.clone(),
                    None => None,
                };

                sender.send((worker_eval, worker_best_move));
            });

            loop {
                if start.elapsed() >= max_duration.or(Some(Duration::ZERO)).unwrap()
                    && depth > min_depth.or(Some(1)).unwrap()
                {
                    //Tell the worker thread to stop
                    stop_sender.send(true).unwrap();
                    //Wait for it to really stop
                    //future.join();
                    break 'outer;
                }

                if let Ok((worker_eval, worker_best_move)) = receiver.try_recv() {
                    eval = worker_eval;
                    best_move = worker_best_move;
                    /*For debugging: */
                    let table_lock = Arc::clone(&table_lock);
                    let table = table_lock.lock().unwrap();
                    let mut board_clone = self.clone();

                    if best_move.is_some() {
                        println!(
                            "Depth {}: {:#?}. Best move: {}",
                            depth,
                            eval,
                            best_move.as_ref().unwrap()
                        );
                    } else {
                        println!("Depth {}: {:#?}. Best move: None", depth, eval);
                    }

                    let mut entry = board_clone.get_transposition_entry(&table).cloned();
                    while entry.is_some() {
                        let m = entry.unwrap().best_move;
                        if m.is_some() {
                            print!("{} ", m.as_ref().unwrap());
                            board_clone.make(&m.unwrap());
                            entry = board_clone.get_transposition_entry(&table).cloned();
                        } else {
                            break;
                        }
                    }
                    println!();

                    break;
                }
            }

            //Only break if we have fully searched at least to a depth of 1.
            depth += 1;

            if eval == i32::MAX || eval == -i32::MAX {
                eprintln!("Stop deepening because forced mate has been found");
                break;
            }
        }

        println!(
            "Searched to a depth of {} plies in {} ms.",
            depth - 1,
            start.elapsed().as_millis()
        );

        (eval, best_move)
    }
}
