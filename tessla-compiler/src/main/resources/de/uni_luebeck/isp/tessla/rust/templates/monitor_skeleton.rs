// This Rust code was automatically created by tessla-compiler from a TeSSLa Specification
// #![allow(unused_parens, unused_variables, non_snake_case, non_camel_case_types, uncommon_codepoints, non_upper_case_globals)]

extern crate tessla_stdlib;

use tessla_stdlib::*;

//USERINCLUDES

pub struct State {
    pub current_ts: i64,
    pub last_processed_ts: i64,
//STATEDEF
}

//STATIC

pub fn get_initial_state() -> State {
//STATESTATIC
    State {
        current_ts: 0,
        last_processed_ts: 0,
//STATEINIT
    }
}

pub fn flush(state: &mut State) {
    step(state, state.current_ts, true);
}

pub fn step(state: &mut State, new_input_ts: i64, flush: bool) {
    let mut flush_required = flush;

    if new_input_ts > state.current_ts || flush_required {
        let mut do_processing = true;
        while do_processing {

//TIMESTAMP

            if state.current_ts == new_input_ts && !flush_required {
                do_processing = false;
            } else {

//COMPUTATION

//DELAYRESET

//STORE

                flush_required = flush && (state.current_ts != new_input_ts);
                state.last_processed_ts = state.current_ts;
                state.current_ts = new_input_ts;
            }
        }
    } else if new_input_ts < state.current_ts {
        panic!("{}: FATAL: decreasing timestamp received", state.current_ts);
    }
}