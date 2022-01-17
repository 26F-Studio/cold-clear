use enum_map::Enum;
use enumset::{enum_set, EnumSet, EnumSetType};
use serde::{Deserialize, Serialize};

use crate::{Board, Row};

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct FallingPiece {
    pub kind: PieceState,
    pub x: i32,
    pub y: i32,
    pub tspin: TspinStatus,
}

impl FallingPiece {
    #[inline]
    pub fn cells(&self) -> [(i32, i32); 4] {
        let mut cells = self.kind.cells();
        for (dx, dy) in cells.iter_mut() {
            *dx += self.x;
            *dy += self.y;
        }
        cells
    }

    #[inline]
    pub fn cells_with_connections(&self) -> [(i32, i32, EnumSet<Direction>); 4] {
        let mut cells = self.kind.cells_with_connections();
        for (dx, dy, _) in cells.iter_mut() {
            *dx += self.x;
            *dy += self.y;
        }
        cells
    }

    pub fn shift<R: Row>(&mut self, board: &Board<R>, dx: i32, dy: i32) -> bool {
        self.x += dx;
        self.y += dy;
        if board.obstructed(self) {
            self.x -= dx;
            self.y -= dy;
            false
        } else {
            self.tspin = TspinStatus::None;
            true
        }
    }

    pub fn sonic_drop<R: Row>(&mut self, board: &Board<R>) -> bool {
        let drop_by = self
            .cells()
            .iter()
            .map(|&(x, y)| y - board.column_heights()[x as usize])
            .min()
            .unwrap();
        if drop_by > 0 {
            self.tspin = TspinStatus::None;
            self.y -= drop_by;
            true
        } else if drop_by < 0 {
            let mut fell = false;
            loop {
                self.y -= 1;
                if board.obstructed(self) {
                    self.y += 1;
                    break;
                }
                fell = true;
                self.tspin = TspinStatus::None;
            }
            fell
        } else {
            false
        }
    }

    fn rotate<R: Row>(&mut self, target: PieceState, board: &Board<R>) -> bool {
        let initial = *self;
        self.kind = target;
        // let initial_offsets = initial.kind.rotation_points();
        // let target_offsets = target.rotation_points();
        //let kicks = initial_offsets
        //    .iter()
        //    .zip(target_offsets.iter())
        //    .map(|(&(x1, y1), &(x2, y2))| (x1 - x2, y1 - y2));
        let table = initial.kind.trs_kick_table(target.1);
        let kicks = table.iter();

        for (i, (dx, dy)) in kicks.enumerate() {
            self.x = initial.x + dx;
            self.y = initial.y + dy;
            if !board.obstructed(self) {
                let immobile = {
                    let mut immobile = true;
                    let moves = [(0, -1), (0, 1), (-1, 0), (1, 0)];
                    for (dx, dy) in moves.iter() {
                        self.x += dx;
                        self.y += dy;
                        if !board.obstructed(self) {
                            immobile = false;
                        }
                        self.x -= dx;
                        self.y -= dy;
                    }
                    immobile
                };
                let three_corner = {
                    let mut corners = 0;
                    for &(dx, dy) in &target.1.mini_tspin_corners() {
                        if board.occupied(self.x + dx, self.y + dy) {
                            corners += 1;
                        }
                    }
                    for &(dx, dy) in &target.1.non_mini_tspin_corners() {
                        if board.occupied(self.x + dx, self.y + dy) {
                            corners += 1;
                        }
                    }
                    corners >= 3
                };
                let non_second = i != 1;
                let use_three_corner = match target.0 {
                    Piece::I | Piece::O => false,
                    _ => true,
                };
                self.tspin = if use_three_corner && immobile != three_corner {
                    if non_second { TspinStatus::Full } else { TspinStatus::Mini }
                } else {
                    if immobile { TspinStatus::Full } else { TspinStatus::None }
                };
                return true;
            }
        }

        *self = initial;
        false
    }

    pub fn cw<R: Row>(&mut self, board: &Board<R>) -> bool {
        let mut target = self.kind;
        target.cw();
        self.rotate(target, board)
    }

    pub fn ccw<R: Row>(&mut self, board: &Board<R>) -> bool {
        let mut target = self.kind;
        target.ccw();
        self.rotate(target, board)
    }

    pub fn flip<R: Row>(&mut self, board: &Board<R>) -> bool {
        let mut target = self.kind;
        target.flip();
        self.rotate(target, board)
    }

    pub fn same_location(&self, other: &Self) -> bool {
        if self.kind.0 != other.kind.0 {
            return false;
        }
        let other_cells = other.cells();
        for c in &self.cells() {
            if !other_cells.contains(c) {
                return false;
            }
        }
        true
    }

    pub fn canonical(&self) -> FallingPiece {
        match self.kind.0 {
            Piece::T | Piece::J | Piece::L => *self,
            Piece::O => match self.kind.1 {
                RotationState::North => *self,
                RotationState::East => FallingPiece {
                    kind: PieceState(Piece::O, RotationState::North),
                    y: self.y - 1,
                    ..*self
                },
                RotationState::West => FallingPiece {
                    kind: PieceState(Piece::O, RotationState::North),
                    x: self.x - 1,
                    ..*self
                },
                RotationState::South => FallingPiece {
                    kind: PieceState(Piece::O, RotationState::North),
                    y: self.y - 1,
                    x: self.x - 1,
                    ..*self
                },
            },
            Piece::S | Piece::Z => match self.kind.1 {
                RotationState::North | RotationState::West => *self,
                RotationState::East => FallingPiece {
                    kind: PieceState(self.kind.0, RotationState::West),
                    x: self.x + 1,
                    ..*self
                },
                RotationState::South => FallingPiece {
                    kind: PieceState(self.kind.0, RotationState::North),
                    y: self.y - 1,
                    ..*self
                },
            },
            Piece::I => match self.kind.1 {
                RotationState::North | RotationState::West => *self,
                RotationState::East => FallingPiece {
                    kind: PieceState(Piece::I, RotationState::West),
                    y: self.y - 1,
                    ..*self
                },
                RotationState::South => FallingPiece {
                    kind: PieceState(Piece::I, RotationState::North),
                    x: self.x - 1,
                    ..*self
                },
            },
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum CellColor {
    I,
    O,
    T,
    L,
    J,
    S,
    Z,
    Garbage,
    Unclearable,
    Empty,
}

#[derive(Debug, Hash, EnumSetType, Enum, Serialize, Deserialize)]
pub enum Piece {
    I,
    O,
    T,
    L,
    J,
    S,
    Z,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum RotationState {
    North,
    South,
    East,
    West,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct PieceState(pub Piece, pub RotationState);

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum TspinStatus {
    None,
    Mini,
    Full,
}

impl RotationState {
    pub fn cw(&mut self) {
        use RotationState::*;
        match self {
            North => *self = East,
            East => *self = South,
            South => *self = West,
            West => *self = North,
        }
    }

    pub fn ccw(&mut self) {
        use RotationState::*;
        match self {
            North => *self = West,
            West => *self = South,
            South => *self = East,
            East => *self = North,
        }
    }

    pub fn flip(&mut self) {
        use RotationState::*;
        match self {
            North => *self = South,
            South => *self = North,
            East => *self = West,
            West => *self = East,
        }
    }

    pub fn mini_tspin_corners(self) -> [(i32, i32); 2] {
        use RotationState::*;
        match self {
            North => [(-1, 1), (1, 1)],
            East => [(1, 1), (1, -1)],
            South => [(1, -1), (-1, -1)],
            West => [(-1, -1), (-1, 1)],
        }
    }

    pub fn non_mini_tspin_corners(self) -> [(i32, i32); 2] {
        use RotationState::*;
        match self {
            South => [(-1, 1), (1, 1)],
            West => [(1, 1), (1, -1)],
            North => [(1, -1), (-1, -1)],
            East => [(-1, -1), (-1, 1)],
        }
    }
}

macro_rules! kick {
    ($matchant:expr => $($from:tt -> $to:tt : [$($tts:tt)*])*) => {
        match $matchant {
            $(
                (kick!(@term $from), kick!(@term $to)) => kick!(@enter [(0, 0)] $($tts)*),
            )*
            (_, _) => unreachable!()
        }
    };
    ($matchant:expr; $offsets:tt => $($from:tt -> $to:tt : [$($tts:tt)*])*) => {
        match $matchant {
            $(
                (kick!(@term $from), kick!(@term $to)) => kick!(@offsets [] [$from $to] $offsets $($tts)*),
            )*
            (_, _) => unreachable!()
        }
    };
    (@term 0) => {North};
    (@term 2) => {South};
    (@term R) => {East};
    (@term L) => {West};
    (@offsets [$($r:tt)*] [0 $($to:tt)*] [0[$a:expr, $b:expr] $($offsets:tt)*] $($tts:tt)*) => {
        kick!(@offsets [$($r)* ($a, $b)] [$($to)*] [$($offsets)*] $($tts)*)
    };
    (@offsets [$($r:tt)*] [0 $($to:tt)*] [$t:tt[$a:expr, $b:expr] $($offsets:tt)*] $($tts:tt)*) => {
        kick!(@offsets [$($r)*] [0 $($to)*] [$($offsets)* $t[$a, $b]] $($tts)*)
    };
    (@offsets [$($r:tt)*] [2 $($to:tt)*] [2[$a:expr, $b:expr] $($offsets:tt)*] $($tts:tt)*) => {
        kick!(@offsets [$($r)* ($a, $b)] [$($to)*] [$($offsets)*] $($tts)*)
    };
    (@offsets [$($r:tt)*] [2 $($to:tt)*] [$t:tt[$a:expr, $b:expr] $($offsets:tt)*] $($tts:tt)*) => {
        kick!(@offsets [$($r)*] [2 $($to)*] [$($offsets)* $t[$a, $b]] $($tts)*)
    };
    (@offsets [$($r:tt)*] [L $($to:tt)*] [L[$a:expr, $b:expr] $($offsets:tt)*] $($tts:tt)*) => {
        kick!(@offsets [$($r)* ($a, $b)] [$($to)*] [$($offsets)*] $($tts)*)
    };
    (@offsets [$($r:tt)*] [L $($to:tt)*] [$t:tt[$a:expr, $b:expr] $($offsets:tt)*] $($tts:tt)*) => {
        kick!(@offsets [$($r)*] [L $($to)*] [$($offsets)* $t[$a, $b]] $($tts)*)
    };
    (@offsets [$($r:tt)*] [R $($to:tt)*] [R[$a:expr, $b:expr] $($offsets:tt)*] $($tts:tt)*) => {
        kick!(@offsets [$($r)* ($a, $b)] [$($to)*] [$($offsets)*] $($tts)*)
    };
    (@offsets [$($r:tt)*] [R $($to:tt)*] [$t:tt[$a:expr, $b:expr] $($offsets:tt)*] $($tts:tt)*) => {
        kick!(@offsets [$($r)*] [R $($to)*] [$($offsets)* $t[$a, $b]] $($tts)*)
    };
    (@offsets [($fromx:expr, $fromy:expr) ($tox:expr, $toy:expr)] [] [$($offsets:tt)*] $($tts:tt)*) => {
        kick!(@enter [($tox - $fromx, $toy - $fromy)] $($tts)*)
    };
    (@enter $($t:tt)*) => {kick!(@single 8 $($t)*)};
    (@single 0 [($($dummy:tt)*), $($out:tt)*]) => {[$($out)*]};
    (@single $k:tt [($xoff:expr, $yoff:expr) $($out:tt)*] + $lit1:literal + $lit2:literal $($tts:tt)*) => {
        kick!(@down $k [($xoff, $yoff) $($out)*, ($lit1+$xoff, $lit2+$yoff)] $($tts)*)
    };
    (@single $k:tt [($xoff:expr, $yoff:expr) $($out:tt)*] + $lit1:literal - $lit2:literal $($tts:tt)*) => {
        kick!(@down $k [($xoff, $yoff) $($out)*, ($lit1+$xoff, -$lit2+$yoff)] $($tts)*)
    };
    (@single $k:tt [($xoff:expr, $yoff:expr) $($out:tt)*] - $lit1:literal + $lit2:literal $($tts:tt)*) => {
        kick!(@down $k [($xoff, $yoff) $($out)*, (-$lit1+$xoff, $lit2+$yoff)] $($tts)*)
    };
    (@single $k:tt [($xoff:expr, $yoff:expr) $($out:tt)*] - $lit1:literal - $lit2:literal $($tts:tt)*) => {
        kick!(@down $k [($xoff, $yoff) $($out)*, (-$lit1+$xoff, -$lit2+$yoff)] $($tts)*)
    };
    (@single $k:tt [($($dummy:tt)*), ($($first:tt)*), $($out:tt)*]) => {kick!(@down $k [($($dummy)*), ($($first)*), $($out)*, ($($first)*)])};
    (@down 8 $($tts:tt)*) => {kick!(@single 7 $($tts)*)};
    (@down 7 $($tts:tt)*) => {kick!(@single 6 $($tts)*)};
    (@down 6 $($tts:tt)*) => {kick!(@single 5 $($tts)*)};
    (@down 5 $($tts:tt)*) => {kick!(@single 4 $($tts)*)};
    (@down 4 $($tts:tt)*) => {kick!(@single 3 $($tts)*)};
    (@down 3 $($tts:tt)*) => {kick!(@single 2 $($tts)*)};
    (@down 2 $($tts:tt)*) => {kick!(@single 1 $($tts)*)};
    (@down 1 $($tts:tt)*) => {kick!(@single 0 $($tts)*)};
}

impl PieceState {
    pub fn cw(&mut self) {
        self.1.cw()
    }

    pub fn ccw(&mut self) {
        self.1.ccw()
    }

    pub fn flip(&mut self) {
        self.1.flip()
    }

    /// Returns the cells this piece and orientation occupy relative to rotation point 1, as well
    /// as the connection directions, in no particular order.
    #[inline(always)]
    pub fn cells(&self) -> [(i32, i32); 4] {
        macro_rules! gen_cells {
            ($([$(($x:expr, $y:expr)),*]),*) => {
                [$(
                    [$(($x, $y)),*],   // North
                    [$((-$x, -$y)),*], // South
                    [$(($y, -$x)),*],  // East
                    [$((-$y, $x)),*]   // West
                ),*]
            };
        }
        const CELLS: &'static [[(i32, i32); 4]] = &gen_cells![
            [(-1, 0), (0, 0), (1, 0), (2, 0)],  // I
            [(0, 0), (1, 0), (0, 1), (1, 1)],   // O
            [(-1, 0), (0, 0), (1, 0), (0, 1)],  // T
            [(-1, 0), (0, 0), (1, 0), (1, 1)],  // L
            [(-1, 0), (0, 0), (1, 0), (-1, 1)], // J
            [(-1, 0), (0, 0), (0, 1), (1, 1)],  // S
            [(-1, 1), (0, 1), (0, 0), (1, 0)]   // Z
        ];
        let index = self.0 as usize * 4 + self.1 as usize;
        CELLS[index]
    }

    pub fn cells_with_connections(&self) -> [(i32, i32, EnumSet<Direction>); 4] {
        use Direction::*;
        let rotate = |d: EnumSet<_>| match self.1 {
            RotationState::North => d,
            RotationState::East => d.iter().map(Direction::cw).collect(),
            RotationState::South => d.iter().map(Direction::flip).collect(),
            RotationState::West => d.iter().map(Direction::ccw).collect(),
        };
        let cells = self.cells();
        [
            (
                cells[0].0,
                cells[0].1,
                rotate(match self.0 {
                    Piece::I => enum_set!(Right),
                    Piece::O => enum_set!(Right | Up),
                    Piece::L => enum_set!(Right),
                    Piece::J => enum_set!(Right | Up),
                    Piece::T => enum_set!(Right),
                    Piece::S => enum_set!(Right),
                    Piece::Z => enum_set!(Right),
                }),
            ),
            (
                cells[1].0,
                cells[1].1,
                rotate(match self.0 {
                    Piece::I => enum_set!(Left | Right),
                    Piece::O => enum_set!(Left | Up),
                    Piece::L => enum_set!(Left | Right),
                    Piece::J => enum_set!(Left | Right),
                    Piece::T => enum_set!(Left | Right | Up),
                    Piece::S => enum_set!(Left | Up),
                    Piece::Z => enum_set!(Left | Down),
                }),
            ),
            (
                cells[2].0,
                cells[2].1,
                rotate(match self.0 {
                    Piece::I => enum_set!(Left | Right),
                    Piece::O => enum_set!(Right | Down),
                    Piece::L => enum_set!(Left | Up),
                    Piece::J => enum_set!(Left),
                    Piece::T => enum_set!(Left),
                    Piece::S => enum_set!(Down | Right),
                    Piece::Z => enum_set!(Up | Right),
                }),
            ),
            (
                cells[3].0,
                cells[3].1,
                rotate(match self.0 {
                    Piece::I => enum_set!(Left),
                    Piece::O => enum_set!(Left | Down),
                    Piece::L => enum_set!(Down),
                    Piece::J => enum_set!(Down),
                    Piece::T => enum_set!(Down),
                    Piece::S => enum_set!(Left),
                    Piece::Z => enum_set!(Left),
                }),
            ),
        ]
    }

    /// Returns the five rotation points associated with this piece and orientation.
    ///
    /// Note that the first point is always (0, 0). We include it here to make
    /// looping over the possible kicks easier.
    pub fn rotation_points(&self) -> [(i32, i32); 5] {
        use Piece::*;
        use RotationState::*;
        match (self.0, self.1) {
            (O, North) => [(0, 0); 5],
            (O, East) => [(0, -1); 5],
            (O, South) => [(-1, -1); 5],
            (O, West) => [(-1, 0); 5],

            (I, North) => [(0, 0), (-1, 0), (2, 0), (-1, 0), (2, 0)],
            (I, East) => [(-1, 0), (0, 0), (0, 0), (0, 1), (0, -2)],
            (I, South) => [(-1, 1), (1, 1), (-2, 1), (1, 0), (-2, 0)],
            (I, West) => [(0, 1), (0, 1), (0, 1), (0, -1), (0, 2)],

            // The rotation points for T, L, J, S, Z are all the same.
            (_, North) => [(0, 0); 5],
            (_, East) => [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)],
            (_, South) => [(0, 0); 5],
            (_, West) => [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)],
        }
    }

     pub fn trs_kick_table(&self, target: RotationState) -> [(i32, i32); 8] {
        use Piece::*;
        use RotationState::*;
        let kicks = match self.0 {
            I => kick!{ (self.1, target);
                    [0[0, 0] R[1, 0] L[0, -1] 2[1, -1]] =>
                0->R: [+0+0 +0+1 +1+0 -2+0 -2-1 +1+2]
                R->0: [+0+0 +2+0 -1+0 -1-2 +2+1 +0+1]
                0->L: [+0+0 +0+1 -1+0 +2+0 +2-1 -1+2]
                L->0: [+0+0 -2+0 +1+0 +1-2 -2+1 +0+1]
                R->2: [+0+0 -1+0 +2+0 +2-1 +0-1 -1+2]
                2->R: [+0+0 -2+0 +1+0 +1-2 -2+1 +0+1]
                L->2: [+0+0 +1+0 -2+0 -2-1 +0-1 +1+2]
                2->L: [+0+0 +2+0 -1+0 -1-2 +2+1 +0+1]
                0->2: [+0+0 -1+0 +1+0 +0-1 +0+1]
                2->0: [+0+0 +1+0 -1+0 +0+1 +0-1]
                R->L: [+0+0 +0-1 -1+0 +1+0 +0+1]
                L->R: [+0+0 +0-1 +1+0 -1+0 +0+1]
            },
            O => [(0, 0); 8],
            T => kick!{ (self.1, target) =>
                0->R: [+0+0 -1+0 -1+1 +0-2 -1-2 +0+1]
                R->0: [+0+0 +1+0 +1-1 +0+2 +1+2 +0-1]
                0->L: [+0+0 +1+0 +1+1 +0-2 +1-2 +0+1]
                L->0: [+0+0 -1+0 -1-1 +0+2 -1+2 +0-1]
                R->2: [+0+0 +1+0 +1-1 +0-1 -1-1 +0+2 +1+2 +1+1]
                2->R: [+0+0 -1+0 +0-2 -1-2 -1-1 +1+1]
                L->2: [+0+0 -1+0 -1-1 +0-1 +1-1 +0+2 -1+2 -1+1]
                2->L: [+0+0 +1+0 +0-2 +1-2 +1-1 -1+1]
                0->2: [+0+0 -1+0 +1+0 +0+1]
                2->0: [+0+0 +1+0 -1+0 +0-1]
                R->L: [+0+0 +0-1 +0+1 +1+0 +0-2 +0+2]
                L->R: [+0+0 +0-1 +0+1 -1+0 +0-2 +0+2]
            },
            S => kick!{ (self.1, target) =>
                0->R: [+0+0 -1+0 -1+1 +0-2 -1-1 -1-2]
                R->0: [+0+0 +1+0 +1-1 +0+2 +1+2 +0-1]
                0->L: [+0+0 +1+0 +1+1 +0-2 +1+2 +0+1]
                L->0: [+0+0 -1+0 -1-1 +0+2 -1-2 -1-2]
                R->2: [+0+0 +1+0 +1-1 +0+2 +1+2 +0-1]
                2->R: [+0+0 -1+0 -1+1 +0-2 -1-2 +0+1]
                L->2: [+0+0 -1+0 -1-1 +0+2 -1+2 -1+1]
                2->L: [+0+0 +1+0 +1+1 +0-2 +1-2 +1-1]
                0->2: [+0+0 -1+0 +1+0 +0-1 +0+1]
                2->0: [+0+0 +1+0 -1+0 +0+1 +0-1]
                R->L: [+0+0 +0+1 +0-1 +0+2]
                L->R: [+0+0 +0-1 +0+1 +0-2]
            },
            Z => kick!{ (self.1, target) =>
                0->R: [+0+0 -1+0 -1+1 +0-2 -1+2 +0+1]
                R->0: [+0+0 +1+0 +1-1 +0+2 +1-2 +1-2]
                0->L: [+0+0 +1+0 +1+1 +0-2 +1-1 +1-2]
                L->0: [+0+0 -1+0 -1-1 +0+2 -1+2 +0-1]
                R->2: [+0+0 +1+0 +1-1 +0+2 +1+2 +1+1]
                2->R: [+0+0 -1+0 -1+1 +0-2 -1-2 -1-1]
                L->2: [+0+0 -1+0 -1-1 +0+2 -1+2 +0-1]
                2->L: [+0+0 +1+0 +1+1 +0-2 +1-2 +0+1]
                0->2: [+0+0 +1+0 -1+0 +0-1 +0+1]
                2->0: [+0+0 -1+0 +1+0 +0+1 +0-1]
                R->L: [+0+0 +0-1 +0+1 +0-2]
                L->R: [+0+0 +0+1 +0-1 +0+2]
            },
            L => kick!{ (self.1, target) =>
                0->R: [+0+0 -1+0 -1+1 +0-2 -1-2 -1-1 +0+1]
                R->0: [+0+0 +1+0 +1-1 +0+2 +1+2 +0-1 +1+1]
                0->L: [+0+0 +1+0 +1+1 +0-2 -1+1 +0+1 +0-1]
                L->0: [+0+0 -1+0 -1-1 +0+2 +1-1 +0-1 +0+1]
                R->2: [+0+0 +1+0 +1-1 -1+0 +0+2 +1+2 +1+1]
                2->R: [+0+0 -1+0 -1-1 +1+0 -1+1 +0-2 -1-2]
                L->2: [+0+0 -1+0 -1-1 -1+1 +1+0 +0-1 +0+2 -1+2]
                2->L: [+0+0 +1+0 +1+1 +1-1 -1+0 +0+1 +0-2 +1-2]
                0->2: [+0+0 +1+0 -1+0 +0-1 +0+1]
                2->0: [+0+0 -1+0 +1+0 +0+1 +0-1]
                R->L: [+0+0 +0+1 +0-1 +1+0]
                L->R: [+0+0 +0-1 +0+1 -1+0]
            },
            J => kick!{ (self.1, target) =>
                0->R: [+0+0 -1+0 -1+1 +0-2 +1+1 +0+1 +0-1]
                R->0: [+0+0 +1+0 +1-1 +0+2 -1-1 +0-1 +0+1]
                0->L: [+0+0 +1+0 +1+1 +0-2 +1-2 +1-1 +0+1]
                L->0: [+0+0 -1+0 -1-1 +0+2 -1+2 +0-1 -1+1]
                R->2: [+0+0 +1+0 +1-1 +1+1 -1+0 +0-1 +0+2 +1+2]
                2->R: [+0+0 -1+0 -1+1 -1-1 +1+0 +0+1 +0-2 -1-2]
                L->2: [+0+0 -1+0 -1-1 +1+0 +0+2 -1+2 -1+1]
                2->L: [+0+0 +1+0 +1-1 -1+0 +1+1 +0-2 +1-2]
                0->2: [+0+0 -1+0 +1+0 +0-1 +0+1]
                2->0: [+0+0 +1+0 -1+0 +0+1 +0-1]
                R->L: [+0+0 +0-1 +0+1 +1+0]
                L->R: [+0+0 +0+1 +0-1 -1+0]
            }
        };
        return kicks;
    }
}

impl rand::distributions::Distribution<Piece> for rand::distributions::Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Piece {
        match rng.gen_range(0, 7) {
            0 => Piece::I,
            1 => Piece::T,
            2 => Piece::O,
            3 => Piece::L,
            4 => Piece::J,
            5 => Piece::S,
            6 => Piece::Z,
            _ => unreachable!(),
        }
    }
}

impl Piece {
    pub fn to_char(self) -> char {
        match self {
            Piece::I => 'I',
            Piece::T => 'T',
            Piece::O => 'O',
            Piece::L => 'L',
            Piece::J => 'J',
            Piece::S => 'S',
            Piece::Z => 'Z',
        }
    }

    pub fn color(self) -> CellColor {
        match self {
            Piece::I => CellColor::I,
            Piece::T => CellColor::T,
            Piece::O => CellColor::O,
            Piece::L => CellColor::L,
            Piece::J => CellColor::J,
            Piece::S => CellColor::S,
            Piece::Z => CellColor::Z,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum PieceMovement {
    Left,
    Right,
    Cw,
    Ccw,
    Flip,
    SonicDrop,
}

impl PieceMovement {
    pub fn apply(self, piece: &mut FallingPiece, board: &Board) -> bool {
        match self {
            PieceMovement::Left => piece.shift(board, -1, 0),
            PieceMovement::Right => piece.shift(board, 1, 0),
            PieceMovement::Ccw => piece.ccw(board),
            PieceMovement::Cw => piece.cw(board),
            PieceMovement::Flip => piece.flip(board),
            PieceMovement::SonicDrop => piece.sonic_drop(board),
        }
    }
}

#[derive(EnumSetType, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn cw(self) -> Direction {
        match self {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        }
    }

    fn ccw(self) -> Direction {
        match self {
            Direction::Up => Direction::Left,
            Direction::Right => Direction::Up,
            Direction::Down => Direction::Right,
            Direction::Left => Direction::Down,
        }
    }

    fn flip(self) -> Direction {
        match self {
            Direction::Up => Direction::Down,
            Direction::Right => Direction::Left,
            Direction::Down => Direction::Up,
            Direction::Left => Direction::Right,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum SpawnRule {
    Row19Or20,
    Row21AndFall,
    RowVariable,
}

impl SpawnRule {
    pub fn spawn<R: Row>(self, piece: Piece, board: &Board<R>) -> Option<FallingPiece> {
        match self {
            SpawnRule::Row19Or20 => {
                let mut spawned = FallingPiece {
                    kind: PieceState(piece, RotationState::North),
                    x: 4,
                    y: 19,
                    tspin: TspinStatus::None,
                };
                if !board.obstructed(&spawned) {
                    return Some(spawned);
                }
                spawned.y += 1;
                if !board.obstructed(&spawned) {
                    return Some(spawned);
                }
            }
            SpawnRule::Row21AndFall => {
                let mut spawned = FallingPiece {
                    kind: PieceState(piece, RotationState::North),
                    x: 4,
                    y: 21,
                    tspin: TspinStatus::None,
                };
                if !board.obstructed(&spawned) {
                    spawned.shift(board, 0, -1);
                    return Some(spawned);
                }
            }
            SpawnRule::RowVariable => {
                let spawned = FallingPiece {
                    kind: PieceState(piece, RotationState::North),
                    x: 4,
                    y: board.spawn,
                    tspin: TspinStatus::None,
                };
                if !board.obstructed(&spawned) {
                    return Some(spawned);
                }
            }
        }
        None
    }
}
