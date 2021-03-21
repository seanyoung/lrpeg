use std::path::PathBuf;

fn main() {
    lrpeg::process_files(&PathBuf::from("src"));
}
