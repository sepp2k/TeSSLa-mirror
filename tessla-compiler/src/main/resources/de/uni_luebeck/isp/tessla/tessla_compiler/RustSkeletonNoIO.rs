// This Rust code was automatically created by tessla-compiler from a TeSSLa Specification
// #![allow(unused_parens, unused_assignments, unused_variables, non_snake_case, dead_code)]

//USERINCLUDES

fn main() {

//STATIC

//VARIABLES

    loop {
        // get input
        input();

        // do step
        flush();
    }
}

fn flush() {
    step(currTs, true);
}

fn step(new_input_ts: i64, flush: bool) {
    let mut flush_required = flush;

    if new_input_ts > currTs || flush_required {

//STORE

        let mut do_processing = true;
        while do_processing {

//TIMESTAMP

            if currTs == new_input_ts && !flush_required {
                do_processing = false;
            } else {

//COMPUTATION

//OUTPUT

                flush_required = flush && (currTs != new_input_ts);
                lastProcessedTs = currTs;
                currTs = new_input_ts;
            }
        }
    } else if new_input_ts < currTs {
        panic!("{}: FATAL: decreasing timestamp received", currTs);
    }
}