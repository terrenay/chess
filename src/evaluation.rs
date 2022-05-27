//#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]

use crate::*;
//All piece tables are from white's perspective.
//They give an offset (in centipawns) from the piece's base value.

//From: https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function

#[allow(dead_code)]
pub enum EvaluationType {
    Exact,
    UpperBound,
    LowerBound,
}

#[rustfmt::skip]
const MG_PAWN_TABLE: [[i32; 8]; 8] = [
    [  0,   0,   0,   0,   0,   0,  0,   0],
    [ 98, 134,  61,  95,  68, 126, 34, -11],
    [ -6,   7,  26,  31,  65,  56, 25, -20],
    [-14,  13,   6,  21,  23,  12, 17, -23],
    [-27,  -2,  -5,  12,  17,   6, 10, -25],
    [-26,  -4,  -4, -10,   3,   3, 33, -12],
    [-35,  -1, -20, -23, -15,  24, 38, -22],
    [  0,   0,   0,   0,   0,   0,  0,   0]
];

#[rustfmt::skip]
const EG_PAWN_TABLE: [[i32; 8]; 8] = [
    [  0,   0,   0,   0,   0,   0,   0,   0],
    [178, 173, 158, 134, 147, 132, 165, 187],
    [ 94, 100,  85,  67,  56,  53,  82,  84],
    [ 32,  24,  13,   5,  -2,   4,  17,  17],
    [ 13,   9,  -3,  -7,  -7,  -8,   3,  -1],
    [  4,   7,  -6,   1,   0,  -5,  -1,  -8],
    [ 13,   8,   8,  10,  13,   0,   2,  -7],
    [  0,   0,   0,   0,   0,   0,   0,   0],
];

#[rustfmt::skip]
const MG_KNIGHT_TABLE: [[i32; 8]; 8] = [
    [-167, -89, -34, -49,  61, -97, -15, -107],
    [ -73, -41,  72,  36,  23,  62,   7,  -17],
    [ -47,  60,  37,  65,  84, 129,  73,   44],
    [  -9,  17,  19,  53,  37,  69,  18,   22],
    [ -13,   4,  16,  13,  28,  19,  21,   -8],
    [ -23,  -9,  12,  10,  19,  17,  25,  -16],
    [ -29, -53, -12,  -3,  -1,  18, -14,  -19],
    [-105, -21, -58, -33, -17, -28, -19,  -23],
];

#[rustfmt::skip]
const EG_KNIGHT_TABLE: [[i32; 8]; 8] = [
    [-58, -38, -13, -28, -31, -27, -63, -99],
    [-25,  -8, -25,  -2,  -9, -25, -24, -52],
    [-24, -20,  10,   9,  -1,  -9, -19, -41],
    [-17,   3,  22,  22,  22,  11,   8, -18],
    [-18,  -6,  16,  25,  16,  17,   4, -18],
    [-23,  -3,  -1,  15,  10,  -3, -20, -22],
    [-42, -20, -10,  -5,  -2, -20, -23, -44],
    [-29, -51, -23, -15, -22, -18, -50, -64],
];

#[rustfmt::skip]
const MG_BISHOP_TABLE: [[i32; 8]; 8] = [
    [-29,   4, -82, -37, -25, -42,   7,  -8],
    [-26,  16, -18, -13,  30,  59,  18, -47],
    [-16,  37,  43,  40,  35,  50,  37,  -2],
    [ -4,   5,  19,  50,  37,  37,   7,  -2],
    [ -6,  13,  13,  26,  34,  12,  10,   4],
    [  0,  15,  15,  15,  14,  27,  18,  10],
    [  4,  15,  16,   0,   7,  21,  33,   1],
    [-33,  -3, -14, -21, -13, -12, -39, -21],
];

#[rustfmt::skip]
const EG_BISHOP_TABLE: [[i32; 8]; 8] = [
    [-14, -21, -11,  -8, -7,  -9, -17, -24],
    [ -8,  -4,   7, -12, -3, -13,  -4, -14],
    [  2,  -8,   0,  -1, -2,   6,   0,   4],
    [ -3,   9,  12,   9, 14,  10,   3,   2],
    [ -6,   3,  13,  19,  7,  10,  -3,  -9],
    [-12,  -3,   8,  10, 13,   3,  -7, -15],
    [-14, -18,  -7,  -1,  4,  -9, -15, -27],
    [-23,  -9, -23,  -5, -9, -16,  -5, -17],
];

#[rustfmt::skip]
const MG_ROOK_TABLE: [[i32; 8]; 8] = [
    [ 32,  42,  32,  51, 63,  9,  31,  43],
    [ 27,  32,  58,  62, 80, 67,  26,  44],
    [ -5,  19,  26,  36, 17, 45,  61,  16],
    [-24, -11,   7,  26, 24, 35,  -8, -20],
    [-36, -26, -12,  -1,  9, -7,   6, -23],
    [-45, -25, -16, -17,  3,  0,  -5, -33],
    [-44, -16, -20,  -9, -1, 11,  -6, -71],
    [-19, -13,   1,  17, 16,  7, -37, -26],
];

#[rustfmt::skip]
const EG_ROOK_TABLE: [[i32; 8]; 8] = [
    [13, 10, 18, 15, 12,  12,   8,   5],
    [11, 13, 13, 11, -3,   3,   8,   3],
    [ 7,  7,  7,  5,  4,  -3,  -5,  -3],
    [ 4,  3, 13,  1,  2,   1,  -1,   2],
    [ 3,  5,  8,  4, -5,  -6,  -8, -11],
    [-4,  0, -5, -1, -7, -12,  -8, -16],
    [-6, -6,  0,  2, -9,  -9, -11,  -3],
    [-9,  2,  3, -1, -5, -13,   4, -20],
];

#[rustfmt::skip]
const MG_QUEEN_TABLE: [[i32; 8]; 8] = [
    [-28,   0,  29,  12,  59,  44,  43,  45],
    [-24, -39,  -5,   1, -16,  57,  28,  54],
    [-13, -17,   7,   8,  29,  56,  47,  57],
    [-27, -27, -16, -16,  -1,  17,  -2,   1],
    [ -9, -26,  -9, -10,  -2,  -4,   3,  -3],
    [-14,   2, -11,  -2,  -5,   2,  14,   5],
    [-35,  -8,  11,   2,   8,  15,  -3,   1],
    [ -1, -18,  -9,  10, -15, -25, -31, -50],
];

#[rustfmt::skip]
const EG_QUEEN_TABLE: [[i32; 8]; 8] = [
    [ -9,  22,  22,  27,  27,  19,  10,  20],
    [-17,  20,  32,  41,  58,  25,  30,   0],
    [-20,   6,   9,  49,  47,  35,  19,   9],
    [  3,  22,  24,  45,  57,  40,  57,  36],
    [-18,  28,  19,  47,  31,  34,  39,  23],
    [-16, -27,  15,   6,   9,  17,  10,   5],
    [-22, -23, -30, -16, -16, -23, -36, -32],
    [-33, -28, -22, -43,  -5, -32, -20, -41],
];

#[rustfmt::skip]
const MG_KING_TABLE: [[i32; 8]; 8] = [
    [-65,  23,  16, -15, -56, -34,   2,  13],
    [ 29,  -1, -20,  -7,  -8,  -4, -38, -29],
    [ -9,  24,   2, -16, -20,   6,  22, -22],
    [-17, -20, -12, -27, -30, -25, -14, -36],
    [-49,  -1, -27, -39, -46, -44, -33, -51],
    [-14, -14, -22, -46, -44, -30, -15, -27],
    [  1,   7,  -8, -64, -43, -16,   9,   8],
    [-15,  36,  12, -54,   8, -28,  24,  14],
];

#[rustfmt::skip]
const EG_KING_TABLE: [[i32; 8]; 8] = [
    [-74, -35, -18, -18, -11,  15,   4, -17],
    [-12,  17,  14,  17,  17,  38,  23,  11],
    [ 10,  17,  23,  15,  20,  45,  44,  13],
    [ -8,  22,  24,  27,  26,  33,  26,   3],
    [-18,  -4,  21,  24,  27,  23,   9, -11],
    [-19,  -3,  11,  21,  23,  16,   7,  -9],
    [-27, -11,   4,  13,  14,   4,  -5, -17],
    [-53, -34, -21, -11, -28, -14, -24, -43]
];

const fn mg_table(kind: PieceKind) -> &'static [[i32; 8]; 8] {
    match kind {
        PieceKind::Pawn => &MG_PAWN_TABLE,
        PieceKind::Knight => &MG_KNIGHT_TABLE,
        PieceKind::Bishop => &MG_BISHOP_TABLE,
        PieceKind::Rook => &MG_ROOK_TABLE,
        PieceKind::Queen => &MG_QUEEN_TABLE,
        PieceKind::King => &MG_KING_TABLE,
    }
}

const fn eg_table(kind: PieceKind) -> &'static [[i32; 8]; 8] {
    match kind {
        PieceKind::Pawn => &EG_PAWN_TABLE,
        PieceKind::Knight => &EG_KNIGHT_TABLE,
        PieceKind::Bishop => &EG_BISHOP_TABLE,
        PieceKind::Rook => &EG_ROOK_TABLE,
        PieceKind::Queen => &EG_QUEEN_TABLE,
        PieceKind::King => &EG_KING_TABLE,
    }
}

const fn mg_value(kind: PieceKind) -> i32 {
    match kind {
        PieceKind::Pawn => 82,
        PieceKind::Knight => 337,
        PieceKind::Bishop => 365,
        PieceKind::Rook => 477,
        PieceKind::Queen => 1025,
        PieceKind::King => 0,
    }
}

const fn eg_value(kind: PieceKind) -> i32 {
    match kind {
        PieceKind::Pawn => 94,
        PieceKind::Knight => 281,
        PieceKind::Bishop => 297,
        PieceKind::Rook => 512,
        PieceKind::Queen => 936,
        PieceKind::King => 0,
    }
}

///How much a piece counts for the game phase. The fewer pieces on the board, the smaller
/// the game phase value should be. This is used to decide whether we are in the end game.
const fn gamephase_value(kind: PieceKind) -> i32 {
    match kind {
        PieceKind::Pawn | PieceKind::King => 0,
        PieceKind::Knight | PieceKind::Bishop => 1,
        PieceKind::Rook => 2,
        PieceKind::Queen => 4,
    }
}

///If not_end_of_game, it is assumed that this position is neither checkmate nor draw. This will be not verified.
/// <b> do not call on a position where current player wins checkmate!</b>
pub fn evaluate_rel(state: &mut BoardState, assume_not_end_of_game: bool) -> i32 {
    if !assume_not_end_of_game {
        if let (Some(eval), _) = end_of_game(state, None) {
            return eval;
        }
    }

    let board = &state.board;
    let mut mg_white = 0;
    let mut mg_black = 0;
    let mut eg_white = 0;
    let mut eg_black = 0;

    //Game phase is initially 24. If one player loses his queen, the count is decreased by 4.
    //If only pawns and kings remain, the count is 0.
    let mut gamephase = 0;

    for rank in 1..=8 {
        for file in 1..=8 {
            if let Square::Full(p) = board.at(rank, file) {
                let col = file - 1;
                gamephase += gamephase_value(p.kind);
                if p.color == PieceColor::White {
                    //Normal direction
                    let row = 8 - rank;
                    mg_white += mg_value(p.kind) + mg_table(p.kind)[row as usize][col as usize];
                    eg_white += eg_value(p.kind) + eg_table(p.kind)[row as usize][col as usize];
                } else {
                    //Flip the tables horizontally for black!
                    let row = rank - 1;
                    mg_black += mg_value(p.kind) + mg_table(p.kind)[row as usize][col as usize];
                    eg_black += eg_value(p.kind) + eg_table(p.kind)[row as usize][col as usize];
                }
            }
        }
    }
    // eprintln!("It's {}'s turn", state.turn);
    // eprintln!("White mg_score: {}", mg_white);
    // eprintln!("Black mg_score: {}", mg_black);

    let mg_score = if state.turn == PieceColor::White {
        mg_white - mg_black
    } else {
        mg_black - mg_white
    };

    let eg_score = if state.turn == PieceColor::White {
        eg_white - eg_black
    } else {
        eg_black - eg_white
    };

    //Now we interpolate between a gamephase value of >=24 (exclusively middle game)
    //and a gamephase value of 0 (exclusively end game).
    let mg_phase = Ord::min(gamephase, 24);
    let eg_phase = 24 - mg_phase;

    (mg_score * mg_phase + eg_score * eg_phase) / 24
}

///If legal_moves is Some(ms), then legal_moves have already been computed (as in negamax).
/// If they have not been precomputed, generates and returns them
pub fn checkmate(
    state: &mut BoardState,
    legal_moves: Option<Vec<Move>>,
) -> (Option<i32>, Option<Vec<Move>>) {
    // eprintln!("start checkmate");
    match legal_moves {
        Some(legal_moves) => {
            // eprintln!("legal moves given");
            if state.check(state.turn) && legal_moves.is_empty() {
                // eprintln!("mate");
                (Some(-i32::MAX), Some(legal_moves))
            } else {
                // eprintln!("not mate, moves: {}", legal_moves.first().unwrap());
                (None, Some(legal_moves))
            }
        }
        None => {
            // eprintln!("legal moves NOT given");
            /*Important in quiescence: We want to avoid generating all moves, so we first make sure we are even in check. */
            if !state.check(state.turn) {
                // eprintln!("not check");
                return (None, None);
            }

            // eprintln!("check, generating all legal moves...");

            let legal_moves = state.generate_legal_moves(true, false);

            if legal_moves.is_empty() {
                // eprintln!("mate");
                (Some(-i32::MAX), Some(legal_moves))
            } else {
                // eprintln!("not mate, moves: {}", legal_moves.first().unwrap());
                (None, Some(legal_moves))
            }
        }
    }
}

/*negamax precomputes legal_moves and passes Some(lm) into this function, which returns (_,None).
quiescence DOESNT precompute legal moves, instead passes None. Only if the position is check, this function
computes all legal moves and returns (_,Some(lm)), which quiescence then filters for captures. */

///If legal_moves are precomputed, pass them in. Otherwise, pass in None and this function computes them only if necessary
/// (that is, if check() returns true), in which case it will return them.
pub fn end_of_game(
    state: &mut BoardState,
    legal_moves: Option<Vec<Move>>,
) -> (Option<i32>, Option<Vec<Move>>) {
    // state.draw_board(true);
    // eprintln!("start end_of_game in position above");
    let (mate_eval, legal_moves) = checkmate(state, legal_moves);

    if mate_eval.is_some() {
        // eprintln!("end end_of_game (checkmate)");
        return (mate_eval, legal_moves);
    }

    draw(state, legal_moves, true)
}

///This function checks for the following conditions:
/// <ul>
/// <li>Stalemate</li>
/// <li>Threefold Repetition</li>
/// <li>Insufficient material (only kings)</li>
/// </ul>
/// <b>If given_not_checkmate, this assumes the current position is not a checkmate situation.</b>
fn draw(
    state: &mut BoardState,
    legal_moves: Option<Vec<Move>>,
    assume_not_checkmate: bool,
) -> (Option<i32>, Option<Vec<Move>>) {
    //If only kings remain, it doesn't matter how many legal moves there are, it's always a draw.
    // eprintln!("start draw");
    if state.taken.len() == 30 {
        return (Some(0), legal_moves);
    }

    if state.threefold_repetition() {
        return (Some(0), legal_moves);
    }

    //If there are 0 legal moves and we know we are not in a checkmate situation, it must be stalemate.
    //Avoid recomputing whether the position is checkmate if we already know it.

    if let Some(legal_moves) = legal_moves {
        // eprintln!("legal moves given");
        if legal_moves.is_empty() {
            if assume_not_checkmate {
                // eprintln!("stalemate");
                return (Some(0), Some(legal_moves));
            } else {
                unimplemented!()
            }
        }
        // eprintln!("not stalemate");
        (None, Some(legal_moves))
    } else if no_legal_moves(state) {
        // eprintln!("legal moves NOT given but lazily computed and no move found");
        if assume_not_checkmate {
            (Some(0), None)
        } else {
            unimplemented!()
        }
    } else {
        // eprintln!("legal moves NOT given but lazily computed and found a move");
        (None, None)
    }
}

///pseudolegal moves müssten auch lazily generiert werden, damit das sinn ergibt!
///
/// Lächerlich ineffizient, macht alles kaputt!
/// todo: bauern, knight etc der reihe nach, und jedes mal dazwischen maken und schauen ob es legal ist. sofort abbrechen beim
/// ersten legalen.
fn no_legal_moves(state: &mut BoardState) -> bool {
    for m in state.generate_pseudo_legal_moves(false, false) {
        state.make(&m);
        if !state.check(state.turn.opposite()) {
            state.unmake();
            return false;
        }
        state.unmake();
    }
    true
}
