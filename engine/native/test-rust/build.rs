fn main() {
    let lib_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../target/scala-3.8.2");
    println!("cargo:rustc-link-search=native={}", lib_dir.display());
    println!("cargo:rustc-link-lib=dylib=petradb-engine");
    println!("cargo:rustc-link-arg=-Wl,-rpath,{}", lib_dir.display());
}
