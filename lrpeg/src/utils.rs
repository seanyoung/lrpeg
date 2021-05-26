pub fn escape_char(ch: char) -> String {
    match ch {
        '\\' => String::from("'\\'"),
        '\t' => String::from("'\\t'"),
        '\n' => String::from("'\\n'"),
        '\r' => String::from("'\\r'"),
        '\'' => String::from("'\\''"),
        ch => format!("'{}'", ch),
    }
}

pub fn escape_string(str: &str) -> String {
    let mut res = String::new();

    for ch in str.chars() {
        match ch {
            '\\' => res.push_str("\\\\"),
            '\t' => res.push_str("\\t"),
            '\n' => res.push_str("\\n"),
            '\r' => res.push_str("\\t"),
            '"' => res.push_str("\\\""),
            ch => res.push(ch),
        }
    }

    res
}
