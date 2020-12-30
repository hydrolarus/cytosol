use goldentests::*;

#[test]
fn goldentests() -> TestResult<()> {
    let config = goldentests::TestConfig::new("../target/debug/cytosolc", "../tests", "// ")?;
    config.run_tests()
}
