use libtetris::Piece;
use mlua::prelude::*;

struct CCBot(cold_clear::Interface);
trait ToPiece {
    fn to_piece(self) -> Piece;
}

impl ToPiece for LuaInteger {
    fn to_piece(self) -> Piece {
        match self {
            1 => Piece::I,
            2 => Piece::O,
            3 => Piece::T,
            4 => Piece::J,
            5 => Piece::L,
            6 => Piece::S,
            7 => Piece::Z,
            _ => panic!("invalid piece"),
        }
    }
}

fn luatable_to_field(table: LuaTable) -> [[bool; 10]; 40] {
    let mut ret = [[false; 10]; 40];
    for i in 0..40 {
        for j in 0..10 {
            ret[i][j] = table.get(i * 10 + j).unwrap();
        }
    }
    ret
}

fn get_dest(m: &libtetris::Move) -> [[LuaInteger; 2]; 4] {
    let mut ret = [[0, 0]; 4];
    for (i, &(x, y)) in m.expected_location.cells().iter().enumerate() {
        ret[i][0] = LuaInteger::from(x);
        ret[i][1] = LuaInteger::from(y);
    }
    ret
}

impl LuaUserData for CCBot {
    fn add_methods<'lua, M: LuaUserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_method_mut("reset", |_, this, (field, b2b_active, combo)| {
            this.0
                .reset(luatable_to_field(field), b2b_active, combo, 0, 0, 0);
            Ok(())
        });
        methods.add_method_mut("addNext", |_, this, pieceid: LuaInteger| {
            this.0.add_next_piece(pieceid.to_piece());
            Ok(())
        });
        methods.add_method_mut("think", |_, this, incoming: Option<u32>| {
            this.0.suggest_next_move(incoming.unwrap_or_default());
            Ok(())
        });
        methods.add_method_mut("getMove", |lua, this, ()| match this.0.poll_next_move() {
            // result,dest,hold,move,b2b,attack,extra,spawn=pcall(ccBot.getMove,ccBot)
            Ok((m, info)) => {
                let plan = &info.plan()[0];
                let lock_result = &plan.1;
                let dest = get_dest(&m);
                let hold = m.hold;
                let b2b = lock_result.b2b;
                let attack = lock_result.garbage_sent;
                let extra = (lock_result.placement_kind.extra()
                    + lock_result.perfect_clear.extra())
                .floor();
                let spawn = match info {
                    cold_clear::Info::Normal(info) => info.spawn,
                    _ => 0,
                };
                return Ok((
                    0,
                    Some(dest),
                    Some(hold),
                    Some(m.inputs),
                    Some(b2b),
                    Some(attack),
                    Some(extra),
                    Some(spawn),
                ));
            }
            Err(cold_clear::BotPollState::Waiting) => {
                return Ok((1, None, None, None, None, None, None, None));
            }
            Err(cold_clear::BotPollState::Dead) => {
                return Ok((2, None, None, None, None, None, None, None));
            }
        });
        methods.add_method_mut("blockNextMove", |_, this, ()| Ok(()));
    }
}

fn about(_: &Lua, _: ()) -> LuaResult<String> {
    Ok("lua wrapper by 26F-Studio".to_owned())
}

#[mlua::lua_module]
fn my_module(lua: &Lua) -> LuaResult<LuaTable> {
    let exports = lua.create_table()?;
    exports.set("about", lua.create_function(about)?)?;
    exports.set(
        "launchAsync",
        lua.create_function(|_, ()| {
            let board = libtetris::Board::new();
            let options = cold_clear::Options::default();
            let weights = cold_clear::evaluation::Standard::default();
            Ok(CCBot(cold_clear::Interface::launch(
                board, options, weights, None,
            )))
        })?,
    )?;
    Ok(exports)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(4, 4);
    }
}
