use arrayvec::ArrayVec;
use serde::{Deserialize, Serialize};
use std::cmp::Ord;

use crate::piece::TspinStatus;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct LockResult {
    pub placement_kind: PlacementKind,
    pub locked_out: bool,
    pub b2b: u32,
    pub perfect_clear: ClearKind,
    pub pc_combo: u32,
    pub combo: Option<u32>,
    pub garbage_sent: u32,
    pub no_combo_sent: u32,
    pub cleared_lines: ArrayVec<[i32; 4]>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum ClearKind {
    None,
    Half,
    All,
}

impl ClearKind {
    pub fn garbage(self, perfect_combo: u32, other_garbage: f32) -> f32 {
        use ClearKind::*;
        match self {
            None => other_garbage,
            Half => other_garbage + 4.,
            All => (8. + (2 * perfect_combo) as f32)
                .clamp(8., 16.)
                .max(other_garbage),
        }
    }
    pub fn extra(self) -> f32 {
        use ClearKind::*;
        match self {
            None => 0.,
            Half => 2.,
            All => 2.,
        }
    }
    pub fn new_gauge(self, b2b_gauge: u32, lines_this_match: u32) -> u32 {
        use ClearKind::*;
        match self {
            None => b2b_gauge,
            Half => b2b_gauge + 100,
            All if lines_this_match <= 4 => b2b_gauge,
            All => b2b_gauge + 800,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum B2BKind {
    B1B,
    B2B,
    B3B,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct PlacementKind {
    pub cleared: u32,
    pub tspin: TspinStatus,
    pub b2b: B2BKind,
}

impl PlacementKind {
    /// The amount of garbage this clear kind normally sends.
    pub fn garbage(self) -> f32 {
        use B2BKind::*;
        use TspinStatus::*;
        if self.cleared == 0 {
            0.0
        } else if self.tspin != None {
            let mut garbage = 2.0 * self.cleared as f32;
            if self.b2b == B2B || self.b2b == B3B {
                garbage += match self.cleared {
                    1 => 1.0,
                    2 => 1.0,
                    3 => 2.0,
                    4 => 4.0,
                    5 => 8.0,
                    _ => unreachable!(),
                }
            }
            if self.b2b == B3B {
                garbage += self.cleared as f32 * 0.5
            }
            if self.tspin == Mini {
                garbage *= 0.25
            }
            garbage
        } else if self.cleared >= 4 {
            match self.b2b {
                B1B => self.cleared as f32,
                B2B => self.cleared as f32 + 1.,
                B3B => self.cleared as f32 * 1.5, 
            }
        } else {
            self.cleared as f32 - 0.5
        }
    }

    pub fn extra(self) -> f32 {
        use B2BKind::*;
        use TspinStatus::*;
        if self.cleared == 0 {
            0.0
        } else if self.tspin != None && self.b2b == B3B {
            1.0
        } else if self.cleared >= 4 && self.b2b == B3B {
            1.0
        } else {
            0.0
        }
    }

    pub fn new_gauge(self, b2b_gauge: u32) -> u32 {
        use TspinStatus::*;
        if self.cleared == 0 {
            let theory = if self.tspin != None {
                b2b_gauge + 20
            } else {
                b2b_gauge
            };
            if b2b_gauge > 800 {
                (theory - 40).clamp(800, 1000)
            } else {
                theory.clamp(0, 800)
            }
        } else if self.tspin != None {
            let diff = match self.cleared {
                1 => 50,
                2 => 100,
                3 => 180,
                4 => 800,
                5 => 1000,
                _ => unreachable!(),
            };
            if self.tspin == Mini {
                b2b_gauge + diff / 2
            } else {
                b2b_gauge + diff
            }
        } else if self.cleared >= 4 {
            b2b_gauge
                + match self.cleared {
                    4 => 150,
                    5 => 200,
                    _ => unreachable!(),
                }
        } else {
            b2b_gauge.max(250) - 250
        }
        // no clamp is applied here because temporary result can be out of range
    }

    pub fn is_clear(self) -> bool {
        self.cleared != 0
    }

    pub(crate) fn get(cleared: usize, tspin: TspinStatus, b2b_gauge: u32) -> Self {
        use B2BKind::*;
        use TspinStatus::*;
        PlacementKind {
            cleared: cleared as u32,
            tspin: tspin,
            b2b: if tspin == None && cleared < 4 || cleared == 0 {
                B1B
            } else if b2b_gauge > 800 {
                B3B
            } else if b2b_gauge >= 50 {
                B2B
            } else {
                B1B
            },
        }
    }

    pub fn name(self) -> String {
        [
            if self.tspin == TspinStatus::Mini {
                "mini"
            } else {
                ""
            },
            match self.b2b {
                B2BKind::B1B => "",
                B2BKind::B2B => "B2B",
                B2BKind::B3B => "B2B2B",
            },
            if self.tspin != TspinStatus::None {
                "Spin"
            } else {
                ""
            },
            match self.cleared {
                0 => "",
                1 => "Single",
                2 => "Double",
                3 => "Triple",
                4 => "Techrash",
                5 => "Techrash+",
                _ => unreachable!(),
            },
        ]
        .iter()
        .copied()
        .filter(|x| !x.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
    }

    pub fn short_name(self) -> String {
        return self.name();
    }
}

impl Default for PlacementKind {
    fn default() -> Self {
        PlacementKind {
            cleared: 0,
            tspin: TspinStatus::None,
            b2b: B2BKind::B1B,
        }
    }
}

pub fn combo_garbage(combo: u32, other_garbage: f32, cleared: u32) -> f32 {
    other_garbage * (1. + combo.clamp(0, 12) as f32 * if cleared == 1 { 0.15 } else { 0.25 })
        + if combo >= 3 && cleared > 0 { 1. } else { 0. }
}
