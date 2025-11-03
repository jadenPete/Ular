use std::path::PathBuf;

fn main() {
    let output_directory = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    let llvm_source = std::env::var("LLVM_SOURCE")
        .map(PathBuf::from)
        .expect("Expected the `LLVM_SOURCE` environment variable to be defined.");

    let runtimes_source = llvm_source.join("runtimes");
    let runtimes_install = cmake::Config::new(runtimes_source.to_str().unwrap())
        .define("CMAKE_ASM_COMPILER", "clang")
        .define("CMAKE_C_COMPILER", "clang")
        .define("CMAKE_CXX_COMPILER", "clang++")
        .define("LLVM_ENABLE_RUNTIMES", "libunwind")
        .build();

    let lib_directory = runtimes_install.join("lib");

    println!("cargo:rustc-link-search=native={}", lib_directory.display());

    // Linking LLVM's libunwind with `--whole-archive` forces all symbols to be included. This ensures
    // that its symbols are preferred over those from the system unwind library (usually `libgcc_s`).
    println!("cargo:rustc-link-arg=-Wl,--whole-archive");
    println!(
        "cargo:rustc-link-arg={}/libunwind.a",
        lib_directory.display()
    );

    println!("cargo:rustc-link-arg=-Wl,--no-whole-archive");

    // See `ular_register_frame` for more information on why we're doing this
    println!("cargo:rustc-link-arg=-Wl,--wrap=__register_frame");
    println!("cargo:rustc-link-arg=-Wl,--wrap=__deregister_frame");

    let include_directory = runtimes_install.join("include");
    let bindings = bindgen::Builder::default()
        .header(include_directory.join("libunwind.h").to_str().unwrap())
        .header(
            llvm_source
                .join("libunwind")
                .join("src")
                .join("libunwind_ext.h")
                .to_str()
                .unwrap(),
        )
        .clang_arg(format!("-I{}", include_directory.display()))
        .clang_arg("-x")
        .clang_arg("c++")
        .allowlist_function("unw_.*")
        .allowlist_function("__unw_.*")
        .allowlist_type("unw_.*")
        .allowlist_var("UNW_.*")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .unwrap();

    bindings
        .write_to_file(output_directory.join("bindings.rs"))
        .unwrap();

    println!("cargo:rerun-if-changed=build.rs");
}
