pub const KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield",
];

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
