//#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
use crate::*;
use rand::prelude::*;
use rand_chacha::ChaCha8Rng;

///This struct only stores the random numbers. It is later used by ZobristState
/// to modify its hash.
#[derive(Clone)]
pub struct ZobristValues {
    //Castling will change white_king, white_rook and both castling rights
    white_king_castle_rights: u64,
    white_queen_castle_rights: u64,
    black_king_castle_rights: u64,
    black_queen_castle_rights: u64,
    black_to_move: u64,
    piece_at: [[[u64; 8]; 8]; 12], //file, rank, piece_index
    en_passant: [[u64; 8]; 8],
}

impl ZobristValues {
    ///Fill in all fields with pseudo random numbers (deterministically)
    pub fn new() -> Self {
        //We need to generate a unique hash for every (Piece, Position) tuple!
        //That is, each (piece_kind, piece_color, rank, file) quadruple needs its own value.
        let mut rng = ChaCha8Rng::seed_from_u64(42);

        let mut piece_at = [[[0; 8]; 8]; 12];

        #[allow(clippy::needless_range_loop)]
        for piece in 0..12 {
            for rank in 0..8 {
                for file in 0..8 {
                    piece_at[piece][rank][file] = rng.next_u64();
                }
            }
        }

        let white_king_castle_rights = rng.next_u64();
        let black_king_castle_rights = rng.next_u64();
        let white_queen_castle_rights = rng.next_u64();
        let black_queen_castle_rights = rng.next_u64();

        let black_to_move = rng.next_u64();

        let mut en_passant = [[0; 8]; 8];

        #[allow(clippy::needless_range_loop)]
        for rank in 0..8 {
            for file in 0..8 {
                en_passant[rank][file] = rng.next_u64();
            }
        }

        Self {
            white_king_castle_rights,
            white_queen_castle_rights,
            black_king_castle_rights,
            black_queen_castle_rights,
            black_to_move,
            piece_at,
            en_passant,
        }
    }

    pub fn get(&mut self, field: Field, piece: Piece) -> u64 {
        self.piece_at[piece.index()][field.rank as usize - 1][field.file as usize - 1]
    }

    fn get_en_passant(&self, field: Field) -> u64 {
        self.en_passant[field.rank as usize - 1][field.file as usize - 1]
    }
}

#[derive(Clone)]
pub struct ZobristState {
    pub values: ZobristValues,
    pub hash: u64,
}

impl ZobristState {
    ///The hash will initially be 0.
    pub fn new() -> Self {
        let values = ZobristValues::new();
        Self { values, hash: 0 } //temporary object, hash to be modified
    }

    pub fn from(
        board: &Board,
        turn: PieceColor,
        castling_rights: &CastlingRights,
        en_passant: Option<Field>,
    ) -> Self {
        //Create an initial object with a hash of 0. The hash of the current
        //board state can then be computed iteratively.
        let mut zobrist_state = Self::new();
        for rank in 1..=8 {
            for file in 1..=8 {
                if let Square::Full(p) = board.at(rank, file) {
                    zobrist_state.change_piece(Field::new(rank, file), p);
                }
            }
        }

        if turn == PieceColor::Black {
            zobrist_state.change_black_to_move();
        }

        if castling_rights.white_king_castle_lost_ply.is_some() {
            zobrist_state.change_castling_rights(MoveType::CastleKingside, PieceColor::White);
        }

        if castling_rights.white_queen_castle_lost_ply.is_some() {
            zobrist_state.change_castling_rights(MoveType::CastleQueenside, PieceColor::White);
        }

        if castling_rights.black_king_castle_lost_ply.is_some() {
            zobrist_state.change_castling_rights(MoveType::CastleKingside, PieceColor::Black);
        }

        if castling_rights.black_queen_castle_lost_ply.is_some() {
            zobrist_state.change_castling_rights(MoveType::CastleQueenside, PieceColor::Black);
        }

        if let Some(ep) = en_passant {
            zobrist_state.change_en_passant(ep);
        }

        zobrist_state
    }

    pub fn from_board_state(board_state: &BoardState) -> Self {
        Self::from(
            &board_state.board,
            board_state.turn,
            &board_state.castling_rights,
            board_state.en_passant(),
        )
    }

    ///Call this function for every piece that is initially present.
    pub fn change_piece(&mut self, field: Field, piece: Piece) {
        // eprintln!("change piece {} {:?}", field, piece);
        self.hash ^= self.values.get(field, piece);
    }

    ///If white to move initially, do not call this function.
    pub fn change_black_to_move(&mut self) {
        // eprintln!("change turn color");
        self.hash ^= self.values.black_to_move;
    }

    ///Only call this function for LOST castling rights!
    pub fn change_castling_rights(&mut self, move_type: MoveType, color: PieceColor) {
        // eprintln!("change castling rights {:?} {}", move_type, color);
        self.hash ^= match move_type {
            MoveType::CastleKingside => match color {
                PieceColor::White => self.values.white_king_castle_rights,
                PieceColor::Black => self.values.black_king_castle_rights,
            },
            MoveType::CastleQueenside => match color {
                PieceColor::White => self.values.white_queen_castle_rights,
                PieceColor::Black => self.values.black_queen_castle_rights,
            },
            _ => panic!("Invalid move type for change_castling_rights"),
        }
    }

    ///Call if this square is available as en passant
    pub fn change_en_passant(&mut self, field: Field) {
        self.hash ^= self.values.get_en_passant(field);
    }
}
