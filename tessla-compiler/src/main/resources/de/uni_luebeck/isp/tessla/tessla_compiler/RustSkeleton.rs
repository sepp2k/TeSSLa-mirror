// This Rust code was automatically created by tessla-compiler from a TeSSLa Specification
// #![allow(unused_parens, unused_assignments, unused_variables, non_snake_case, dead_code)]

extern crate tessla_stdlib;
use tessla_stdlib::*;

//USERINCLUDES

struct State {
    current_ts: i64,
    last_processed_ts: i64,
//STATEDEF
}

//STATIC

fn get_initial_state() -> State {
    State {
        current_ts: 0,
        last_processed_ts: 0,
//STATEINIT
    }
}

fn main() {
    let mut new_input_ts = 0;
    let mut input_stream_name = String::new();
    let mut input_stream_value = String::new();

    let state = &mut get_initial_state();

    loop {

        if tessla_stdlib::parse_input(&mut input_stream_name, &mut input_stream_value, &mut new_input_ts) {
            break; // reached EOF
        }

        if new_input_ts != state.current_ts {
            step(state, new_input_ts, false);
        }

//INPUT

    }

    flush(state);
}//ENDMAIN

fn flush(state: &mut State) {
    step(state, state.current_ts, true);
}

fn step(state: &mut State, new_input_ts: i64, flush: bool) {
    let mut flush_required = flush;

    if new_input_ts > state.current_ts || flush_required {

        let mut do_processing = true;
        while do_processing {

//TIMESTAMP

            if state.current_ts == new_input_ts && !flush_required {
                do_processing = false;
            } else {

//COMPUTATION

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