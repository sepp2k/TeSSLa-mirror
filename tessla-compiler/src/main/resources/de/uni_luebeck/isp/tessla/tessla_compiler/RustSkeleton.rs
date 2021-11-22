// This Rust code was automatically created by tessla-compiler from a TeSSLa Specification
// #![allow(unused_parens, unused_assignments, unused_variables, non_snake_case, dead_code)]

mod rust_stdlib;
use rust_stdlib::*;

//USERINCLUDES

fn main() {
    let mut input_stream_name: String = String::new();
    let mut input_stream_value: String = String::new();

//STATIC

//VARIABLES

    loop {
        let reached_eof = parse_input(&mut input_stream_name, &mut input_stream_value);

        inputStream = input_stream_name.as_str();
        value = input_stream_value.as_str();

        if newInputTs > currTs || reached_eof {
            if reached_eof {
                newInputTs += 1;
            }

//STORE

            let mut do_processing = true;
            while do_processing {

//TIMESTAMP

                if currTs == newInputTs {
                    do_processing = false;
                } else {

//COMPUTATION

//OUTPUT

                    lastProcessedTs = currTs;
                    currTs = newInputTs;
                }
            }

            if reached_eof {
                exit(0);
            }
        } else if newInputTs < currTs {
            panic!("{}: FATAL: decreasing timestamp received", currTs);
        }

//INPUT

    }
}