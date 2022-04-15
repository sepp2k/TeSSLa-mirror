extern crate monitor;

use monitor::*;

fn main() {
    let mut new_input_ts = 0;
    let mut input_stream_name = String::new();
    let mut input_stream_value = String::new();

    let state = &mut get_initial_state();

    loop {
        if parse_input(&mut input_stream_name, &mut input_stream_value, &mut new_input_ts) {
            break; // reached EOF
        }

        if new_input_ts != state.current_ts {
            step(state, new_input_ts, false);
        }

//INPUT

    }

    flush(state);
}