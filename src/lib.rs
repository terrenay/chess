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

//next up: multithreading: neuer thread für jeden der ersten moves einer position (als erste idee)
//wichtigstes feature: pawn promotion
//danach castling
//(und previous moves wieder zeichnen!)

//---> todo: white_king position updaten wenn der könig sich bewegt!!! Momentan
//bleibt er für das programm immer stehen

/*struct AttackMap {
    map: Array2<bool>,
}

impl AttackMap {
    /// <b>Precondition: -1 <= rank, file <= 10
    fn at(&self, rank: i8, file: i8) -> bool {
        let (i, j) = Self::to_board(rank, file);
        self.map[[i, j]]
    }

    /// <b>Precondition: -1 <= rank, file <= 10
    fn set(&mut self, rank: i8, file: i8, val: bool) {
        let (i, j) = Self::to_board(rank, file);
        self.map[[i, j]] = val;
    }

    fn new() -> Self {
        AttackMap {
            map: Array2::<bool>::default((8, 8)),
        }
    }

    ///Not valid for padding squares, since these don't matter for check checks!
    fn to_board(rank: i8, file: i8) -> (usize, usize) {
        debug_assert!(rank >= 1 && rank <= 8 && file >= 1 && file <= 8);
        (9 - rank as usize, file as usize)
    }
}*/

///Currently the idea is to have a single instance of BoardState that is then modified.
/// The other option would be to clone it each ply, but that seems like a waste of
/// resources.
///
/// For multithreading, I could later clone the BoardState at some node and have each
/// thread modify its own copy downwards.
pub struct BoardState {
    pub board: Board,
    turn: PieceColor,
    ply: u16,
    white_queen_castle_lost_ply: Option<u16>,
    black_queen_castle_lost_ply: Option<u16>,
    white_king_castle_lost_ply: Option<u16>,
    black_king_castle_lost_ply: Option<u16>,

    //en_passant: Option<Field>,
    moves: Vec<Move>,
    taken: Vec<Piece>,
    //white_attack_map: AttackMap, //vlt array2<vec<uniquepiece>> schlussendlich
}

impl BoardState {
    pub fn new() -> Self {
        Self {
            board: Board::new(),
            turn: PieceColor::White,
            ply: 1,
            white_queen_castle_lost_ply: None,
            black_queen_castle_lost_ply: None,
            white_king_castle_lost_ply: None,
            black_king_castle_lost_ply: None,
            //en_passant: None,
            moves: vec![],
            taken: vec![],
            //white_attack_map: AttackMap::new(),
        }
    }

    ///Static evaluation
    ///
    /// Positive: White's advantage
    pub fn evaluate(&self) -> i32 {
        evaluation::evaluate(self)
    }

    ///Assumes root has color state.turn
    pub fn minimax(&mut self, depth: i32) -> Option<Move> {
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
                    //In case of forced checkmate for black, score will always be i32::MIN
                    //(the worst possible outcome for white). Without this, best_move
                    //would stay None, since no move increases the best outcome. But we still
                    //want some move to be played, even if it lead to a forced checkmate.
                    best_move.get_or_insert(m);

                    //If there is a forced checkmate sequence
                    if alpha == i32::MAX || beta == i32::MIN {
                        //todo: wird alpha überhaupt gesetzt für forced checkmate?
                        println!("early return");
                        return best_move;
                    }

                    self.make(m);

                    //It's now black's turn
                    //If this move didn't put white into check, recurse.
                    //If it did put white into check, ignore.
                    if !self.check(self.turn.opposite()) {
                        let score = self.minimax_helper(depth - 1, alpha, beta);
                        moves += 1;
                        if score > best_value {
                            best_move = Some(m);
                            best_value = score;
                        }
                        if score > alpha {
                            alpha = score;
                        }
                    }
                    self.unmake();
                }

                //White has no legal moves in the original position
                if moves == 0 {
                    if self.checkmate_given_zero_moves(PieceColor::Black) {
                        println!("CHECKMATE! BLACK WINS!");
                        debug_assert_eq!(best_value, i32::MIN);
                    } else {
                        println!("STALEMATE! THE GAME IS A DRAW!");
                    }
                }
            }

            //Black is the minimizing player
            PieceColor::Black => {
                let mut best_value = i32::MAX;
                for m in self.generate_moves() {
                    best_move.get_or_insert(m);
                    if alpha == i32::MAX || beta == i32::MIN {
                        return best_move;
                    }
                    self.make(m);
                    //It's now black's turn
                    //If this move didn't put white into check, recurse
                    if !self.check(self.turn.opposite()) {
                        let score = self.minimax_helper(depth - 1, alpha, beta);
                        moves += 1;
                        if score < best_value {
                            best_move = Some(m);
                            best_value = score;
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
                        debug_assert_eq!(best_value, i32::MAX);
                    } else {
                        println!("STALEMATE! THE GAME IS A DRAW!");
                    }
                    return None;
                }
            }
        }
        best_move
    }

    fn minimax_helper(&mut self, depth: i32, mut alpha: i32, mut beta: i32) -> i32 {
        //alpha is the most positive eval that white is already assured of at this point
        //beta is the most negative eval that black is already assured of at this point
        if depth == 0 {
            self.evaluate()
        } else {
            let mut moves = 0;
            match self.turn {
                //White is the maximizing player
                PieceColor::White => {
                    let mut best_value = i32::MIN;
                    for m in self.generate_moves() {
                        //Branch cut: Don't explore this subtree further if it is already
                        //obvious that this variant will not be taken.
                        if beta <= alpha || alpha == i32::MAX || beta == i32::MIN {
                            return best_value;
                        }
                        self.make(m);
                        //It is now black's turn
                        //If this move didn't put white into check, recurse
                        if !self.check(self.turn.opposite()) {
                            let score = self.minimax_helper(depth - 1, alpha, beta);
                            moves += 1;
                            if score > best_value {
                                best_value = score;
                            }
                            if score > alpha {
                                alpha = score;
                            }
                        }
                        self.unmake();
                    }
                    if moves == 0 && !self.checkmate_given_zero_moves(PieceColor::Black) {
                        //This position is stalemate
                        best_value = 0;
                    }
                    best_value
                }
                //Black is the minimizing player
                PieceColor::Black => {
                    let mut best_value = i32::MAX;
                    for m in self.generate_moves() {
                        if beta <= alpha || alpha == i32::MAX || beta == i32::MIN {
                            return best_value;
                        }
                        self.make(m);
                        //It's now black's turn
                        //If this move didn't put white into check, recurse
                        if !self.check(self.turn.opposite()) {
                            let score = self.minimax_helper(depth - 1, alpha, beta);
                            moves += 1;
                            if score < best_value {
                                best_value = score;
                            }
                            if score < beta {
                                beta = score;
                            }
                        }
                        self.unmake();
                    }
                    if moves == 0 && !self.checkmate_given_zero_moves(PieceColor::Black) {
                        //This position is stalemate
                        best_value = 0;
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
    #[allow(clippy::needless_return)]
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
            return Err(Error::BadMoveTarget(Field::new(to_rank, to_file)));
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
            self.white_king_castle_lost_ply.unwrap_or(0)
        );
        println!(
            "White queen: {:?}",
            self.white_queen_castle_lost_ply.unwrap_or(0)
        );
        println!(
            "Black king: {:?}",
            self.black_king_castle_lost_ply.unwrap_or(0)
        );
        println!(
            "Black queen: {:?}",
            self.black_queen_castle_lost_ply.unwrap_or(0)
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
                        todo!("avoid king captures");
                    }
                }
            }
        }
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
                        self.white_king_castle_lost_ply.get_or_insert(self.ply);
                        self.white_queen_castle_lost_ply.get_or_insert(self.ply);
                    } else {
                        self.board.black_king = Field::new(m.to.rank, m.to.file);
                        self.black_king_castle_lost_ply.get_or_insert(self.ply);
                        self.black_queen_castle_lost_ply.get_or_insert(self.ply);
                    }
                }
                PieceKind::Rook => {
                    //We can skip checking the color because the move is assumed to be
                    //legal, so if it is from a1, it must be white's move.
                    if m.from == Field::new(1, 1) {
                        //white queen-side
                        self.white_queen_castle_lost_ply.get_or_insert(self.ply);
                    } else if m.from == Field::new(1, 8) {
                        //white king-side
                        self.white_king_castle_lost_ply.get_or_insert(self.ply);
                    } else if m.from == Field::new(8, 1) {
                        //black queen-side
                        self.black_queen_castle_lost_ply.get_or_insert(self.ply);
                    } else if m.from == Field::new(8, 8) {
                        //black king-side
                        self.black_king_castle_lost_ply.get_or_insert(self.ply);
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
    fn unmake(&mut self) {
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
                if let Some(castle_ply) = self.white_king_castle_lost_ply {
                    if self.ply == castle_ply {
                        self.white_king_castle_lost_ply = None;
                    }
                }
                if let Some(castle_ply) = self.white_queen_castle_lost_ply {
                    if self.ply == castle_ply {
                        self.white_queen_castle_lost_ply = None;
                    }
                }
                if let Some(castle_ply) = self.black_king_castle_lost_ply {
                    if self.ply == castle_ply {
                        self.black_king_castle_lost_ply = None;
                    }
                }
                if let Some(castle_ply) = self.black_queen_castle_lost_ply {
                    if self.ply == castle_ply {
                        self.black_queen_castle_lost_ply = None;
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
    fn checkmate_given_zero_moves(&self, winning_color: PieceColor) -> bool {
        self.check(winning_color.opposite())
    }

    ///Returns all valid castling moves of player whose turn it currently is.
    fn castling_moves(&self) -> Vec<Move> {
        let mut v = vec![];
        //Check cheap conditions (empty, castling rights) first, then expensive (threatened)

        if self.turn == PieceColor::White {
            if self.white_king_castle_lost_ply.is_none()
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
            if self.white_queen_castle_lost_ply.is_none()
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
            if self.black_king_castle_lost_ply.is_none()
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
            if self.black_queen_castle_lost_ply.is_none()
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
                            tile = tile.on_truecolor(122, 156, 70)
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
    fn new(from: Field, to: Field, move_type: MoveType) -> Self {
        Move {
            from,
            to,
            move_type,
            promotion: None,
        }
    }

    ///Generate a new promotion move.
    /// To use only for pawns!
    fn promotion(from: Field, to: Field, move_type: MoveType, p: Piece) -> Self {
        Move {
            from,
            to,
            move_type,
            promotion: Some(p),
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

    fn empty(&self, rank: i8, file: i8) -> bool {
        matches!(self.at(rank, file), Square::Empty)
    }

    ///Only field 1 works at the moment (only position layout, with no further info)
    ///
    /// TODO: king position und castling rigts werden ignoriert!
    fn from_fen(fen: &str) -> Board {
        let mut board = Array2::<Square>::default((12, 12));
        let mut rank = 8;
        let mut file = 1;
        let mut black_king = None;
        let mut white_king = None;

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
                _ => (),
            }
            file += 1;
        }
        Board {
            board,
            white_king: white_king.unwrap(),
            black_king: black_king.unwrap(),
        }
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
                        let mut v = vec![];
                        if rank == 2 {
                            v.push(Move::promotion(
                                from,
                                Field::new(rank - 1, file),
                                MoveType::Default,
                                Piece::queen(PieceColor::Black),
                            ));
                        } else {
                            //Exclusive else because you may not push a pawn to the last
                            //rank without promoting it.
                            v.push(Move::new(
                                from,
                                Field::new(rank - 1, file),
                                MoveType::Default,
                            ));
                        }

                        //Can never be a promotion move
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
                //Remember this could also be a promotion move.
                match self.at(rank - 1, file - 1) {
                    Square::Full(p) if matches!(p.color, PieceColor::White) => {
                        if rank == 2 {
                            v.push(Move::promotion(
                                from,
                                Field::new(rank - 1, file - 1),
                                MoveType::Capture,
                                Piece::queen(PieceColor::Black),
                            ));
                        } else {
                            v.push(Move::new(
                                from,
                                Field::new(rank - 1, file - 1),
                                MoveType::Capture,
                            ));
                        }
                    }
                    _ => (),
                }

                //Black pawn attacks down to the right
                match self.at(rank - 1, file + 1) {
                    //Right attack
                    Square::Full(p) if matches!(p.color, PieceColor::White) => {
                        if rank == 2 {
                            v.push(Move::promotion(
                                from,
                                Field::new(rank - 1, file + 1),
                                MoveType::Capture,
                                Piece::queen(PieceColor::Black),
                            ));
                        } else {
                            v.push(Move::new(
                                from,
                                Field::new(rank - 1, file + 1),
                                MoveType::Capture,
                            ));
                        }
                    }
                    _ => (),
                }
                v
            }
            PieceColor::White => {
                let mut v = match self.at(rank + 1, file) {
                    Square::Empty => {
                        let mut v = vec![];
                        if rank == 7 {
                            v.push(Move::promotion(
                                from,
                                Field::new(rank + 1, file),
                                MoveType::Default,
                                Piece::queen(PieceColor::White),
                            ));
                        } else {
                            v.push(Move::new(
                                from,
                                Field::new(rank + 1, file),
                                MoveType::Default,
                            ));
                        }

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
                        if rank == 7 {
                            v.push(Move::promotion(
                                from,
                                Field::new(rank + 1, file - 1),
                                MoveType::Capture,
                                Piece::queen(PieceColor::White),
                            ));
                        } else {
                            v.push(Move::new(
                                from,
                                Field::new(rank + 1, file - 1),
                                MoveType::Capture,
                            ));
                        }
                    }
                    _ => (),
                }

                //White pawn attacks up to the right
                match self.at(rank + 1, file + 1) {
                    Square::Full(p) if matches!(p.color, PieceColor::Black) => {
                        if rank == 7 {
                            v.push(Move::promotion(
                                from,
                                Field::new(rank + 1, file + 1),
                                MoveType::Capture,
                                Piece::queen(PieceColor::White),
                            ));
                        } else {
                            v.push(Move::new(
                                from,
                                Field::new(rank + 1, file + 1),
                                MoveType::Capture,
                            ));
                        }
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
            .filter(|&&to| {
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

    ///White, Black
    /*fn king_positions(&self) -> (Field, Field) {
        let mut white_king = Field::new(0, 0);
        let mut black_king = Field::new(0, 0);
        for rank in 1..=8 {
            for file in 1..=8 {
                if let Square::Full(p) = self.at(rank, file) {
                    if matches!(p.kind, PieceKind::King) {
                        if p.color == PieceColor::White {
                            white_king = Field::new(rank, file);
                        } else {
                            black_king = Field::new(rank, file);
                        }
                    }
                }
            }
        }
        (white_king, black_king)
    }*/

    ///Example: Converts a to 1.
    fn file_letter_to_number(file: char) -> Result<usize, Error> {
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
pub enum PieceColor {
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
    #[error("piece on {0} has wrong color")]
    WrongColor(Field),
    #[error("cannot move to {0}")]
    BadMoveTarget(Field),
    #[error("cannot parse symbol")]
    Parse,
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
