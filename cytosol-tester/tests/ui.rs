use goldentests::*;

#[test]
fn goldentests() -> TestResult<()> {
    let config = goldentests::TestConfig::new("../target/debug/cytosol-tester", "../tests", "// ")?;
    config.run_tests()
}
