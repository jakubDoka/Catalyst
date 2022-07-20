pub use testing::test_case;

mod testing {
    use diags::*;
    use packaging_t::*;

    pub fn test_case(name: &str, test_code: impl Fn(&mut Workspace, &mut Packages)) {
        let mut ws = Workspace::new();
        let mut packages = Packages::new();
        test_code(&mut ws, &mut packages);
        let mut out = String::new();
        ws.display(&packages, &mut out).unwrap();
        std::fs::write(name, out).unwrap();
    }
}