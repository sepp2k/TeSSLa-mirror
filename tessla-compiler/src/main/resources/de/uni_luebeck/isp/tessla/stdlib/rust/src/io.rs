use std::fmt::Display;
use std::io::stdin;
use std::str::FromStr;

fn find_end(string: &str, delim: &str, start: usize) -> usize {
    if (start + delim.len()) <= string.len() && delim == &string[start..(start + delim.len())] {
        start
    } else if start >= (string.len() - 1) {
        string.len()
    } else {
        find_end(&string, delim, match &string[start..(start + 1)] {
            "(" if delim != "\""  => find_end(&string, ")", start + 1) + 1,
            "{" if delim != "\""  => find_end(&string, "}", start + 1) + 1,
            "\""                  => find_end(&string, "\"", start + 1) + 1,
            "\\" if delim == "\"" => start + 2,
            _                     => start + 1
        })
    }
}

pub fn process_string_input(string: &str) -> String {
    if &string[0..1] == "\"" && find_end(&string, "\"", 1) == (string.len() - 1) {
        string[1..(string.len() - 1)]
            .replace("\\\\n", "\n")
            .replace("\\\\r", "\r")
            .replace("\\\\t", "\t")
            .replace("\\\\\"", "\"")
            .replace("\\\\\\\\", "\\")
    } else {
        panic!("Not a valid string input: \"{}\"", string);
    }
}

pub fn output_var<T: Display>(output: T, true_name: &str, ts: i64, raw: bool) {
    if raw {
        println!("{}", output);
    } else {
        println!("{}: {} = {}", ts, true_name, output);
    }
}

pub fn parse_input(input_stream_name: &mut String, input_stream_value: &mut String, new_input_ts: &mut i64) -> bool {
    let mut line = String::new();
    let mut reached_eof = false;

    match stdin().read_line(&mut line) {
        Err(error) => panic!("ERROR parsing the input stream\n{}", error),
        Ok(0) => reached_eof = true,
        Ok(_) => {
            // TODO do we want to handle all these errors or just use unwrap()?
            let (timestamp, input_expression) = match line.split_once(':') {
                Some(value) => value,
                None => panic!("ERROR parsing input '{}'", line.trim())
            };
            *new_input_ts = match i64::from_str(timestamp) {
                Ok(value) => value,
                Err(err) => panic!("ERROR failed to parse timestamp:\n{}", err)
            };
            match input_expression.split_once('=') {
                Some((lhs, rhs)) => {
                    *input_stream_name = lhs.trim().to_string();
                    *input_stream_value = rhs.trim().to_string();
                },
                None => {
                    *input_stream_name = input_expression.trim().to_string();
                    *input_stream_value = String::new();
                }
            };
        }
    };
    line.clear();

    return reached_eof;
}