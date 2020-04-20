use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

pub fn compile_protos<P: AsRef<Path>>(protos: P, package: P, root: &str) -> io::Result<()> {
    tonic_build::compile_protos(protos)?;
    let out_path = PathBuf::from(std::env::var("OUT_DIR").unwrap())
        .join(package)
        .join(Path::new(".rs"));
    let package_str: String = File::open(out_path).and_then(|mut f| {
        let mut buf = String::new();
        f.read_to_string(&mut buf)?;
        Ok(buf)
    })?;
    let ast = syn::parse_file(&package_str).expect("tonic-build output is malformed");

    // generate around resources:
    // 2. recursive Poll[TypeName] trait
    // 2.'' recursive getters/accessors:
    // not sure how to support this:
    // - resource.backend.big_query.data.table.my_table
    // [x] 2.' recursive Merge[TypeName] impl [covered by swamp-codegen]

    Ok(())
}
