use crate::*;

impl BoardState {
    ///Only allows pseudo-legal moves that do not leave the player in check.
    pub fn generate_legal_moves(
        &mut self,
        sort: bool,
        only_captures: bool,
        transposition_table: Option<&HashMap<u32, TranspositionEntry>>,
    ) -> Vec<Move> {
        let pseudo_legal_moves =
            self.generate_pseudo_legal_moves(sort, only_captures, transposition_table);
        let mut legal_moves = Vec::with_capacity(pseudo_legal_moves.len());
        for m in pseudo_legal_moves.into_iter() {
            self.make(&m);
            if !self.check(self.turn.opposite()) {
                legal_moves.push(m);
            }
            self.unmake();
        }
        legal_moves
    }

    pub fn generate_pseudo_legal_moves(
        &self,
        sort: bool,
        only_captures: bool,
        transposition_table: Option<&HashMap<u32, TranspositionEntry>>,
    ) -> Vec<Move> {
        let mut v: Vec<Move> = if only_captures {
            vec![]
        } else {
            self.castling_moves()
        };

        for rank in 1..=8 {
            for file in 1..=8 {
                if let Square::Full(p) = self.board.at(rank, file) {
                    if p.color == self.turn {
                        v.extend(
                            self.pseudo_legal_moves(Field::new(rank, file), only_captures)
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
            if let Some(table) = transposition_table {
                if let Some(entry) = self.get_transposition_entry(table) {
                    if let Some(shallow_best) = &entry.best_move {
                        //If the tt-entry appears in v, we can be sure it is not an index collision. In this case, we move it to the
                        //beginning to improve alpha-beta-pruning.
                        //We have to remove the old v-entry, since the tt entry might be evicted before the second v-entry.
                        if let Some(old_index) = v.iter().position(|m| m == shallow_best) {
                            v.remove(old_index);
                            v.insert(0, shallow_best.clone());
                        }
                    }
                }
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

    ///Returns all valid castling moves of player whose turn it currently is.
    pub fn castling_moves(&self) -> Vec<Move> {
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

    ///May return NoPieceOnField Error
    pub fn pseudo_legal_moves(&self, from: Field, only_captures: bool) -> Result<Vec<Move>, Error> {
        let Field { rank, file } = from;
        let board = &self.board;
        match board.at(rank, file) {
            Square::Full(p) => match p.kind {
                PieceKind::Pawn => {
                    Ok(self.pseudo_legal_pawn_moves(p.color, from, only_captures, false))
                }
                PieceKind::Knight => {
                    Ok(board.pseudo_legal_knight_moves(p.color, from, only_captures))
                }
                PieceKind::Bishop => {
                    Ok(board.pseudo_legal_bishop_moves(p.color, from, only_captures, false))
                }
                PieceKind::King => Ok(board.pseudo_legal_king_moves(p.color, from, only_captures)),
                PieceKind::Rook => {
                    Ok(board.pseudo_legal_rook_moves(p.color, from, only_captures, false))
                }
                PieceKind::Queen => {
                    Ok(board.pseudo_legal_queen_moves(p.color, from, only_captures))
                }
            },
            _ => Err(Error::NoPieceOnField(Field::new(rank, file))),
        }
    }

    pub fn pseudo_legal_pawn_moves(
        &self,
        color: PieceColor,
        from: Field,
        only_captures: bool,
        no_en_passant: bool,
    ) -> Vec<Move> {
        let Field { rank, file } = from;
        let starting_rank;
        let promotion_rank;
        let rank_offset;
        let board = &self.board;
        let opposite_color = color.opposite();
        let pawn = Piece::pawn(color);
        let mut moves = vec![];
        let left_right = vec![-1, 1];
        match color {
            PieceColor::White => {
                starting_rank = 2;
                promotion_rank = 7;
                rank_offset = 1;
            }
            PieceColor::Black => {
                starting_rank = 7;
                promotion_rank = 2;
                rank_offset = -1;
            }
        }

        //Attack to the left and right.
        //Remember this could also be a promotion move.
        for file_offset in left_right.iter() {
            match board.at(rank + rank_offset, file + file_offset) {
                Square::Full(p) if p.color == opposite_color => {
                    moves.push(Move::promotion(
                        from,
                        Field::new(rank + rank_offset, file + file_offset),
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
        }

        if only_captures && no_en_passant {
            return moves;
        }

        //En passant capture
        if let Some(Field {
            rank: ep_rank,
            file: ep_file,
        }) = self.en_passant()
        {
            debug_assert!(ep_rank == 6 || ep_rank == 3);
            if rank + rank_offset == ep_rank {
                for file_offset in left_right.iter() {
                    if file + file_offset == ep_file
                        && self.board.at(rank, file + file_offset)
                            == Square::Full(Piece::pawn(opposite_color))
                    {
                        moves.push(Move::en_passant(from, color, Field::new(ep_rank, ep_file)));
                    }
                }
            }
        }

        if only_captures {
            return moves;
        }

        if let Square::Empty = board.at(rank + rank_offset, file) {
            moves.push(Move::promotion(
                from,
                Field::new(rank + rank_offset, file),
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
                if let Square::Empty = board.at(rank + 2 * rank_offset, file) {
                    moves.push(Move::new(
                        from,
                        Field::new(rank + 2 * rank_offset, file),
                        pawn,
                        MoveType::Default,
                    ));
                }
            }
        };

        moves
    }

    ///Assumes the move is fully legal!
    pub fn make(&mut self, m: &Move) {
        let old_en_passant = self.en_passant();

        self.moves.push(m.clone());
        let old_castling_rights = self.castling_rights.clone();

        if let Square::Full(moving_piece) = self.board.at(m.from.rank, m.from.file) {
            debug_assert_eq!(moving_piece, m.piece);
            match m.move_type {
                MoveType::Default => {}

                MoveType::Capture => {
                    let target_field = if m.en_passant {
                        m.get_en_passant_taken().unwrap()
                    } else {
                        m.to
                    };

                    if let Square::Full(target) =
                        self.board.at(target_field.rank, target_field.file)
                    {
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
            self.make_zobrist(m, moving_piece, &changed_castling_rights, old_en_passant);

            self.hash_history.push(self.zobrist.hash);

            //The from square is now empty.

            self.board.set(m.from.rank, m.from.file, Square::Empty);

            //Promote if it is a promotion move, otherwise move the original piece
            self.board.set(
                m.to.rank,
                m.to.file,
                Square::Full(m.promotion.unwrap_or(moving_piece)),
            );

            //En passant: Remove the taken pawn separately
            if m.en_passant {
                let victim = m.get_en_passant_taken().unwrap();
                self.board.set(victim.rank, victim.file, Square::Empty)
            }
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
                            let target_field = if m.en_passant {
                                debug_assert_eq!(
                                    taken_piece,
                                    Piece::pawn(moving_piece.color.opposite())
                                );
                                m.get_en_passant_taken().unwrap()
                            } else {
                                m.to
                            };
                            self.board.set(
                                target_field.rank,
                                target_field.file,
                                Square::Full(taken_piece),
                            );
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
}

impl Board {
    pub fn pseudo_legal_knight_moves(
        &self,
        color: PieceColor,
        from: Field,
        only_captures: bool,
    ) -> Vec<Move> {
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
        self.filter_free_or_opponent(from, v, Piece::knight(color), only_captures)
    }

    fn pseudo_legal_king_moves(
        &self,
        color: PieceColor,
        from: Field,
        only_captures: bool,
    ) -> Vec<Move> {
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
        self.filter_free_or_opponent(from, v, Piece::king(color), only_captures)
    }

    pub fn pseudo_legal_rook_moves(
        &self,
        color: PieceColor,
        from: Field,
        only_captures: bool,
        queen: bool,
    ) -> Vec<Move> {
        let Field { rank, file } = from;
        let piece = if queen {
            Piece::queen(color)
        } else {
            Piece::rook(color)
        };
        //Upwards
        let mut v = vec![];
        for i in rank + 1..=8 {
            let to = Field::new(i, file);
            if self.insert_or_break_loop(from, to, piece, &mut v, only_captures) == BreakLoop::True
            {
                break;
            }
        }
        //Downwards
        for i in (1..rank).rev() {
            let to = Field::new(i, file);
            if self.insert_or_break_loop(from, to, piece, &mut v, only_captures) == BreakLoop::True
            {
                break;
            }
        }
        //Right
        for j in file + 1..=8 {
            let to = Field::new(rank, j);
            if self.insert_or_break_loop(from, to, piece, &mut v, only_captures) == BreakLoop::True
            {
                break;
            }
        }
        //Left
        for j in (1..file).rev() {
            let to = Field::new(rank, j);
            if self.insert_or_break_loop(from, to, piece, &mut v, only_captures) == BreakLoop::True
            {
                break;
            }
        }
        v
    }

    pub fn pseudo_legal_bishop_moves(
        &self,
        color: PieceColor,
        from: Field,
        only_captures: bool,
        queen: bool,
    ) -> Vec<Move> {
        let mut v = vec![];
        let mut diff = 1;
        let Field { rank, file } = from;
        let piece = if queen {
            Piece::queen(color)
        } else {
            Piece::bishop(color)
        };

        //Up right
        while rank + diff <= 8 && file + diff <= 8 {
            let to = Field::new(rank + diff, file + diff);
            if self.insert_or_break_loop(from, to, piece, &mut v, only_captures) == BreakLoop::True
            {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Up left
        while rank + diff <= 8 && file - diff >= 1 {
            let to = Field::new(rank + diff, file - diff);
            if self.insert_or_break_loop(from, to, piece, &mut v, only_captures) == BreakLoop::True
            {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Down right
        while rank - diff >= 1 && file + diff <= 8 {
            let to = Field::new(rank - diff, file + diff);
            if self.insert_or_break_loop(from, to, piece, &mut v, only_captures) == BreakLoop::True
            {
                break;
            }
            diff += 1;
        }
        diff = 1;

        //Down left
        while rank - diff >= 1 && file - diff >= 1 {
            let to = Field::new(rank - diff, file - diff);
            if self.insert_or_break_loop(from, to, piece, &mut v, only_captures) == BreakLoop::True
            {
                break;
            }
            diff += 1;
        }
        v
    }

    ///Inefficiency at its peak
    fn pseudo_legal_queen_moves(
        &self,
        color: PieceColor,
        from: Field,
        only_captures: bool,
    ) -> Vec<Move> {
        let mut v = self.pseudo_legal_rook_moves(color, from, only_captures, true);
        v.extend(self.pseudo_legal_bishop_moves(color, from, only_captures, true));
        v
    }
    ///This function is intended to be used in a loop for a sliding piece which follows that
    /// piece's move direction and checks each square for obstruction by an opponent or own piece.
    /// This function returns BreakLoop::True if the current square is full or padding.
    /// It returns BreakLoop::False if the square is empty.
    /// It pushes coordinates into vector v if the color is the opponent's color or if it's empty.
    ///
    /// If only_captures is true, this will only insert moves that are captures into v.
    fn insert_or_break_loop(
        &self,
        from: Field,
        to: Field,
        piece: Piece,
        v: &mut Vec<Move>,
        only_captures: bool,
    ) -> BreakLoop {
        let Field { rank, file } = to;
        match self.at(rank, file) {
            Square::Empty => {
                if !only_captures {
                    v.push(Move::new(from, to, piece, MoveType::Default));
                }
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
    ///
    /// If only_captures is true, this will only return moves that are captures.
    fn filter_free_or_opponent(
        &self,
        from: Field,
        v: Vec<Field>,
        piece: Piece,
        only_captures: bool,
    ) -> Vec<Move> {
        let mut captures: Vec<Move> = v
            .iter()
            .filter(|&to| {
                if let Square::Full(p) = self.at(to.rank, to.file) {
                    p.color == piece.color.opposite()
                } else {
                    false
                }
            })
            .map(|to| Move::new(from, *to, piece, MoveType::Capture))
            .collect();

        if only_captures {
            return captures;
        }

        let default_moves: Vec<Move> = v
            .iter()
            .filter(|to| matches!(self.at(to.rank, to.file), Square::Empty))
            .map(|&to| Move::new(from, to, piece, MoveType::Default))
            .collect();

        captures.extend(default_moves);
        captures
    }
}
