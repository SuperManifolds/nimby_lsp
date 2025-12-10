fn main() {
    // Ensure we recompile when the API definitions change
    println!("cargo:rerun-if-changed=../../api-definitions/nimbyrails.v1.toml");
}
